require 'zlib'
require_relative 'delta'
require_relative 'sha_ref'
require_relative 'packfile'

module GitRead
    class Repository
        def initialize(rootPath)
            @base = rootPath + '.git'
            @packIdxsList = Dir.entries(@base + 'objects/pack/').find_all { |f| f.end_with?('.idx') }
            @packFilesList = Dir.entries(@base + 'objects/pack/').find_all { |f| f.end_with?('.pack') }

            # to be loaded lazily.
            @leftPackToLoad = @packIdxsList
            @loadedIndexFiles = []
        end

        # ShaRef -> (GitObject | nil)
        def access_object(sha)
            loose_file = @base + 'objects/' + sha.loose_path
            if File.exist?(loose_file)
                access_loose_object(sha, loose_file)
            else
                access_packed_object(sha)
            end
        end

        class RawBlob
            attr_reader :obj_type, :data
            def initialize(obj_typ, obj_data)
                @data = obj_data
                @obj_type = obj_typ
            end
        end

        # ShaRef -> (RawBlob | nil)
        def access_object_raw(sha)
            loose_file = @base + 'objects/' + sha.loose_path
            if File.exist?(loose_file)
                RawBlob.new(:unknown, access_loose_object_raw(sha, loose_file))
            else
                access_packed_object_raw(sha)
            end
        end

        # ShaRef
        # Return the SHA1 of the HEAD (if any) or nil if not found.
        def head_sha
            open(@base + 'HEAD', 'r') do |file|
                content = file.read

                if /ref: (.*)/ =~ content.chomp
                    refPath = Regexp.last_match(1)
                    file.close
                    # Loose reference to the ref
                    if File.exists?( @base + refPath )
                        open( @base + refPath, 'r' ) do |ref_file|
                            sha = ShaRef.new( ref_file.read )
                            ref_file.close
                            return sha
                        end
                    # if not loose try the packed reference file
                    elsif File.exists?( @base + 'packed-refs' )
                        open( @base + 'packed-refs', 'r' ) do |file|
                            file.read.lines.each do |line|
                                if /([0-9a-fA-F]{40}) (.*)/ =~ line
                                    sha = Regexp.last_match(1)
                                    v = Regexp.last_match(2)
                                    return ShaRef.new(sha) if v == refPath
                                end
                            end
                        end
                    end
                end
            end
            nil
        end

    private
        # (ShaRef, FilePath) -> GitObject
        # \return a GitRead::Objects corresponding to the given sha
        def access_loose_object(sha, filename)
            data = access_loose_object_raw(sha, filename)
            GitRead.read_loose_object(self, sha, data)
        end

        INFLATE_SIZE_MARGIN = 50

        # (ShaRef, FilePath) -> RawString
        # A loose object is an object stored in a single file
        # in the '.git/objects' subdirectory.
        def access_loose_object_raw(sha, filename)
            open(filename, 'rb') do |file| 
                Zlib::Inflate.inflate(file.read)
            end
        end

        # (ShaRef, PackFileEntryHeader, Int, File, Int) -> RawBlob
        # header is PackFileEntryHeader
        def read_delta_object_raw(sha, header, offset, file, file_size)
            case header.obj_type
            when PackFileEntryHeader::OBJ_OFS_DELTA
                base_offset = DeltaOffset.read(file)
                real_offset = offset - base_offset.to_i

                to_read = [header.uncompressed_size + INFLATE_SIZE_MARGIN,
                           file_size - offset - header.read_size].min

                deflated_data = file.read(to_read)

                # must read _raw_ object, not clean one
                # origin :: RawBlob
                origin = read_packed_object_raw(sha, real_offset, file)

            when PackFileEntryHeader::OBJ_REF_DELTA
                puts "OBJ_REF_DELTA"
                to_read = [header.read_size, file_size - offset - header.read_size].min
                deflated_data = file.read(to_read)

                ref = ShaRef.new(BinSha.read(file).bits)
                origin = access_object_raw(ref)
            else
                throw
            end

            inflated_delta = Zlib::Inflate.inflate(deflated_data)
            full_data = RawDeltaPack.read(inflated_delta).to_delta.apply_delta(origin.data)
            RawBlob.new(origin.obj_type, full_data)
        end

        # (ShaRef, Int, File) -> RawBlob
        def read_packed_object_raw(sha, offset, pack_file)
            pack_file.seek(0, IO::SEEK_END)
            file_size = pack_file.pos

            pack_file.seek(offset, IO::SEEK_SET)
            pack_entry_header = GitRead::PackFileEntryHeader.read(pack_file)

            if pack_entry_header.delta?
                return read_delta_object_raw(sha, pack_entry_header, offset, pack_file, file_size)
            else
                read_size = [pack_entry_header.uncompressed_size + INFLATE_SIZE_MARGIN,
                        file_size - offset].min
                read_data = pack_file.read(read_size + 50)
                return RawBlob.new(pack_entry_header.obj_type, Zlib::Inflate.inflate(read_data))
            end
        end

        def read_packed_object(sha, offset, pack_file)
            raw = GitRead.read_pack_object_raw(sha, pack_entry_header.obj_type, object_data)
            GitRead.read_pack_object(repo, sha, raw[0], raw[1])
        end

        # ShaRef -> (GitObject | nil)
        def access_packed_object(sha)
            raw = access_packed_object_raw(sha)
            return nil if raw.nil?
            GitRead.read_pack_object(self, sha, raw.obj_type, raw.data)
        end

        # ShaRef -> (RawBlob | nil)
        def access_packed_object_raw(sha)
           found_ref = find_packed_ref(sha)
           if !found_ref
               return nil
           end

           packfilename = @base + 'objects/pack/' + @packFilesList[ found_ref[1] ]
           ret = nil

           open(packfilename, 'rb') do |pack_file|
               header = GitRead::PackFileHeader.read(pack_file.read(PackFileHeader::HEADER_SIZE))
               if !header || !header.valid?
                   return nil
               end

               ret = read_packed_object_raw(sha, found_ref[0], pack_file)
           end

           ret
        end

        # ShaRef -> ([Offset, Sizer] | nil)
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

