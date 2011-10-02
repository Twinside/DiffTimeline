require 'pathname'
require_relative 'sha_ref'

module GitRead
    class Blob
        def initialize(sha, data, kind)
            @sha = sha

            case kind
            when :textual
                idx = data.index("\x00")
                if !idx
                    throw
                end

                @data = data.slice!(idx + 1 .. data.length)
            when :binary
                @data = data
            end
        end

        def data
            @data
        end
    end

    class Commit
        # we dont care about kind
        def initialize(sha, data, kind)
            @sha = sha
            @message = ''
            @parents = []
            @author = ''
            @committer = ''
            @tree = nil

            case kind
            when :textual
                idx = data.index("\x00")
                if !idx
                    throw
                end

                # we drop the blob info
                data = data.slice(idx + 1 .. data.length)
                parse_commit(data)
            when :binary
                parse_commit(data)
            end

        end

        def tree
            @tree
        end

        def to_s
            "(#{@sha}) #{@message.lines.first}"
        end

        def parents
            @parents
        end

    private
        def parse_commit(data)
            data.each_line do |line|
                case line
                when /^parent (.*)\n/
                    @parents << ShaRef.new(Regexp.last_match(1))
                when /^author (.*)\n/
                    @author = Regexp.last_match(1)
                when /^committer (.*)\n/
                    @committer = Regexp.last_match(1)
                when /^tree (.*)\n/
                    @tree = ShaRef.new(Regexp.last_match(1))
                when /^\n$/
                    nil
                else
                    @message << line
                end
            end
        end
    end

    class Tree
        def initialize(sha, data, kind)
            @listing = []
            @sha = sha

            case kind
            when :textual
                idx = data.index("\x00")
                if !idx
                    throw
                end

                # we drop the blob info
                data = data.slice(idx + 1 .. data.length)
                parse_tree(data, 0)
            when :binary
                parse_tree(data, 0)
            end
        end

        def access_path(repo, path)
            access_inner_path(repo, Pathname.new(path).each_filename)
        end

    protected
        RIGHTS_IDX = 0
        NAME_IDX = 1
        SHA_IDX = 2

        def access_inner_path(repo, path_enum)
            child_val = ''
            begin
                child_val = path_enum.next
            rescue StopIteration
                return self
            end

            found_iteration = @listing.index { |info| info[NAME_IDX] == child_val }
            return nil if !found_iteration

            child_object = repo.access_object(@listing[found_iteration][SHA_IDX])
            pp child_object.class
            if child_object.class != self.class
                return child_object 
            end

            child_object.access_inner_path(repo, path_enum)
        end

    private
        def parse_tree(data, generalIdx)
            while !data.empty?
                /^(?<rights>\d+)/ =~ data

                idx = data.index("\x00")
                filename = data.slice(rights.length + 1 .. idx - 1)
                sha = data.slice(idx + 1 .. idx + 20).unpack('C20')
                @listing << [rights, filename, ShaRef.new(sha)]

                # cut away what we read
                data = data.slice!(idx + 21, data.length)
            end
        end
    end

    BLOB_PREFIX = "blob "
    COMMIT_PREFIX = "commit "
    TREE_PREFIX = "tree "

    def GitRead.read_pack_object(sha, type, data)
        case type
        when PackFileEntryHeader::OBJ_COMMIT 
            Commit.new(sha, data, :binary)
        when PackFileEntryHeader::OBJ_TREE
            Tree.new(sha, data, :binary)
        when PackFileEntryHeader::OBJ_BLOB
            Blob.new(sha, data, :binary)
        when PackFileEntryHeader::OBJ_TAG
            pp "PackFileEntryHeader::OBJ_TAG"
            nil
        when PackFileEntryHeader::OBJ_OFS_DELTA
            pp "PackFileEntryHeader::OBJ_OFS_DELTA"
            nil
        when PackFileEntryHeader::OBJ_REF_DELTA
            pp "PackFileEntryHeader::OBJ_REF_DELTA"
            nil
        else
            nil
        end
    end

    def GitRead.read_loose_object(sha, str)
        case 
        when str.start_with?(BLOB_PREFIX) 
            Blob.new(sha, str, :textual) 
        when str.start_with?(COMMIT_PREFIX) 
            Commit.new(sha, str, :textual) 
        when str.start_with?(TREE_PREFIX) 
            Tree.new(sha, str, :textual) 
        else 
            nil 
        end
    end
end

