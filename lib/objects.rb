require 'pathname'
require_relative 'sha_ref'

module GitRead
    class Blob
        attr_reader :data, :sha

        def initialize(repo, sha, data, kind)
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
    end

    # Represent a git commit, with the parents, the
    # message, committer and author info and (even more
    # importantly) the GitRead::Tree object of the data.
    class Commit
        attr_reader :sha, :message, :parents_sha, :author, :tree_sha, :commiter
        # kind either :textual or :binary
        def initialize(repo, sha, data, kind)
            @repository = repo
            @sha = sha
            @message = ''
            @parents_sha = []
            @author = ''
            @committer = ''
            @tree_sha = nil

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

        def to_s
            "(#{@sha.to_s[0..5]}) #{@message.lines.first}"
        end

        def tree
            @repository.access_object(@tree_sha)
        end

    private
        def parse_commit(data)
            data.each_line do |line|
                case line
                when /^parent (.*)\n/
                    @parents_sha << ShaRef.new(Regexp.last_match(1))
                when /^author (.*)\n/
                    @author = Regexp.last_match(1)
                when /^committer (.*)\n/
                    @committer = Regexp.last_match(1)
                when /^tree (.*)\n/
                    @tree_sha = ShaRef.new(Regexp.last_match(1))
                when /^\n$/
                    nil
                else
                    @message << line
                end
            end
        end
    end

    # Tree class, storing list of other object with
    # a (rights, name, sha) storage.
    class Tree
        attr_reader :sha

        def initialize(repo, sha, data, kind)
            @repository = repo
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

        # (FilePath) -> (Object | nil)
        # Access a git object in the tree (if any) with the
        # given repository and path.
        def access_path(path)
            file_list = []
            Pathname.new(path).each_filename { |f| file_list << f }
            access_inner_path(@repository, 0, file_list)
        end

    protected
        RIGHTS_IDX = 0
        NAME_IDX = 1
        SHA_IDX = 2

        def access_inner_path(repo, idx, path_array)
            child_val = ''
            if idx >= path_array.size
                return self
            end

            child_val = path_array[idx]

            # linear search, find a way to do a binary search here (abstract it)
            found_iteration = @listing.index { |info| info[NAME_IDX] == child_val }
            return nil if !found_iteration

            child_object = repo.access_object(@listing[found_iteration][SHA_IDX])

            if child_object.class != self.class
                return child_object 
            end

            child_object.access_inner_path(repo, idx + 1, path_array)
        end

    private
        def parse_tree(data, generalIdx)
            while !data.empty?
                /^(\d+)/ =~ data

                rights = Regexp.last_match(1)

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

    # (ShaRef, Type, String) -> (GitObject | nil)
    def GitRead.read_pack_object(repo, sha, obj_type, data)
        case obj_type
        when PackFileEntryHeader::OBJ_COMMIT 
            Commit.new(repo, sha, data, :binary)
        when PackFileEntryHeader::OBJ_TREE
            Tree.new(repo, sha, data, :binary)
        when PackFileEntryHeader::OBJ_BLOB
            Blob.new(repo, sha, data, :binary)
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

    # (ShaRef, String) -> (GitObject | nil)
    def GitRead.read_loose_object(repo, sha, str)
        case 
        when str.start_with?(BLOB_PREFIX) 
            Blob.new(repo, sha, str, :textual) 
        when str.start_with?(COMMIT_PREFIX) 
            Commit.new(repo, sha, str, :textual) 
        when str.start_with?(TREE_PREFIX) 
            Tree.new(repo, sha, str, :textual) 
        else 
            nil 
        end
    end
end

