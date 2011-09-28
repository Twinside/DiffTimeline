require 'zlib'

module GitRead
    class Repository
        def initialize(rootPath)
            @base = rootPath + '/.git/'
            @packIdxs = Dir.entries(@base + 'objects/pack/').find_all { |f| f.end_with?('.idx') }
            @packFiles = Dir.entries(@base + 'objects/pack/').find_all { |f| f.end_with?('.pack') }
        end

        def access_object(sha)
            loose_file = @base + 'objects/' + sha.loose_path
            if File.exist?(loose_file )
                access_loose_object(loose_file)
            else
            end
        end

    private
        # A loose object is an object stored in a single file
        # in the '.git/objects' subdirectory.
        def access_loose_object(filename)
            open(filename, 'rb') do |file| 
                GitRead.readObject(Zlib::Inflate.inflate(file.read))
            end
        end
    end
end

