require 'zlib'
require_relative 'sha_ref'
require_relative 'packfile'

module GitRead
    class Repository
        def initialize(rootPath)
            @base = rootPath + '/.git/'
            @packIdxsList = Dir.entries(@base + 'objects/pack/').find_all { |f| f.end_with?('.idx') }
            @packFilesList = Dir.entries(@base + 'objects/pack/').find_all { |f| f.end_with?('.pack') }

            # to be loaded lazily.
            @leftPackToLoad = @packIdxsList
            @loadedIndexFiles = []
        end

        def access_object(sha)
            loose_file = @base + 'objects/' + sha.loose_path
            if File.exist?(loose_file)
                access_loose_object(loose_file)
            else
                access_packed_object(sha)
            end
        end

        # Return the SHA1 of the HEAD (if any) or nil if not found.
        def head_sha
            open(@base + 'HEAD', 'r') do |file|
                content = file.read

                if /ref: (?<refPath>.*)/ =~ content.chomp
                    file.close
                    open( @base + refPath ) do |ref_file|
                        sha = ShaRef.new( ref_file.read )
                        ref_file.close
                        return sha
                    end
                end
            end
            nil
        end

    private
        # A loose object is an object stored in a single file
        # in the '.git/objects' subdirectory.
        def access_loose_object(filename)
            open(filename, 'rb') do |file| 
                GitRead.readObject(Zlib::Inflate.inflate(file.read))
            end
        end

        def access_packed_object(sha)
           foundRef = find_packed_ref(sha) 
        end

        def find_packed_ref(sha)
            # we search in the alread loaded list
            offset = nil
            idx = 0
            @loadedIndexFiles.each do |pack|
                offset = pack.offset_of(sha)
                break if offset
                idx += 1
            end

            return [offset, idx] if offset

            while @leftPackToLoad.size > 0
                toLoad = @base + 'objects/pack/' + @leftPackToLoad[0]
                pack = open(toLoad, 'rb') do |file|
                    GitRead::PackFileIndex.read(file)
                end

                @loadedIndexFiles << pack
                offset = pack.offset_of(sha)
                return [offset, @leftPackToLoad.size - 1] if offset
                @leftPackToLoad = @leftPackToLoad.slice(1, @leftPackToLoad.size)
            end

            return nil
        end
    end
end

