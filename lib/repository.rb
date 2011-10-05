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
                access_loose_object(sha, loose_file)
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
        def access_loose_object(sha, filename)
            open(filename, 'rb') do |file| 
                GitRead.read_loose_object(sha, Zlib::Inflate.inflate(file.read))
            end
        end

        def access_packed_object(sha)
           foundRef = find_packed_ref(sha)
           if !foundRef
               return "not_found"
           end

           packfilename = @base + 'objects/pack/' + @packFilesList[ foundRef[1] ]
           ret = nil
           header_size = 4 + 4 + 4 + 1
           open(packfilename, 'rb') do |pack_file|
               header = GitRead::PackFileHeader.read(pack_file.read(header_size))

               if !header || !header.valid?
                   return nil
               end

               pack_file.seek(0, IO::SEEK_END)
               file_size = pack_file.pos

               pack_file.seek(foundRef[0], IO::SEEK_SET)
               pack_entry_header = GitRead::PackFileEntryHeader.read(pack_file)



               if pack_entry_header.delta?
                   read_size = pack_entry_header.uncompressed_size
                   object_data = pack_file.read(read_size)
                   pp inflated_data
               else
                   read_size = [pack_entry_header.uncompressed_size,
                            file_size - foundRef[0]].min
                   read_data = pack_file.read(read_size)
                   object_data = Zlib::Inflate.inflate(read_data)
               end

               ret = GitRead.read_pack_object(sha, pack_entry_header.obj_type, object_data)
           end

           ret
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
                @leftPackToLoad = @leftPackToLoad.slice(1, @leftPackToLoad.size)
                return [offset, @leftPackToLoad.size - 1] if offset
            end

            return nil
        end
    end
end

