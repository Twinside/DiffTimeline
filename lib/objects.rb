require_relative 'sha_ref'

module GitRead
    class Blob
        def initialize(data)
            idx = data.index("\x00")
            if !idx
                throw
            end

            @data = data.slice!(idx + 1 .. data.length)
        end
    end

    class Commit
        def parents
            @parents
        end

        def initialize(data)
            @message = ''
            @parents = []
            @author = ''
            @commiter = ''
            data.each_line do |line|
                case line
                when /^commit/
                    nil
                when /^parent (.*)/
                    @parents << ShaRef.new(Regexp.last_match(1))
                when /^author (.*)/
                    @author = Regexp.last_match(1)
                when /^commiter (.*)/
                    @commiter = Regexp.last_match(1)
                else
                    @message << line
                end
            end
        end
    end

    class Tree
        def initialize(data)
            idx = data.index("\x00")
            if !idx
                throw
            end

            # we drop the blob info
            data = data.slice(idx + 1 .. data.length)
            @listing = []
            parseTree(data, 0)
        end

    private
        def parseTree(data, generalIdx)
            while !data.empty?
                rights = data.slice(0 .. 5)
                idx = data.index("\x00")
                filename = data.slice(7 .. idx - 1)
                sha = data.slice(idx + 1 .. idx + 20).unpack('C20')
                @listing << [rights, filename, sha]
                data = data.slice!(idx + 21, data.length)
            end
        end
    end

    BLOB_PREFIX = "blob "
    COMMIT_PREFIX = "commit "
    TREE_PREFIX = "tree "

    def GitRead.readObject(str)
        case 
        when str.start_with?(BLOB_PREFIX) 
            Blob.new(str) 
        when str.start_with?(COMMIT_PREFIX) 
            Commit.new(str) 
        when str.start_with?(TREE_PREFIX) 
            Tree.new(str) 
        else 
            nil 
        end
    end
end

