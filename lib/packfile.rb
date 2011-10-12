require 'bindata'

module GitRead
    # Represent a SHA1 stored in a binary form
    class BinSha < BinData::Record
        array :bits, :type => :uint8, :initial_length => 20
    end

    # Class representing a binary pack
    class PackFileIndex < BinData::Record
        uint32be :head1
        uint32be :head2
        array    :fanout, :type => :uint32be, :initial_length => 255
        uint32be :compcount
        array    :shas, :type => :bin_sha,   :initial_length => :compcount
        array    :crcs, :type => :uint32be, :initial_length => :compcount
        array    :offsets, :type => :uint32be, :initial_length => :compcount
        bin_sha  :packChecksum
        bin_sha  :indexChecksum

        # return the index of the specified sha in the
        # corresponding pack file or nil if not found.
        def offset_of(sha)
            # we access the bounds of the index file
            # by checking data from the fanout table.
            begining = 0
            ending = compcount
            
            if sha.raw[0] > 0
                begining = fanout[ sha.raw[0] - 1]
            end

            if sha.raw[0] < 255
                ending = fanout[ sha.raw[0] ]
            end

            find_between(sha, begining, ending)
        end

    private
        def find_between(sha, beg, ending)
            if ending < beg
                puts "Not found #{sha}"
                return nil
            end
            middle = beg + (ending - beg) / 2

            if shas[middle].bits == sha.raw
                puts "Found #{sha} in packfile at #{offsets[middle]}"
                return offsets[middle]
            end

            if sha.is_greater_than(shas[middle].bits)
                return find_between(sha, middle + 1, ending)
            else
                return find_between(sha, beg, middle - 1)
            end
        end
    end

    # Used to unpack the size of a delta
    class DeltaSize < BinData::Record
        bit1 :a_end
        bit7 :a_size

        bit1 :b_end,     :initial_value => 0, :onlyif => :a_end?
        bit7 :b_size,    :initial_value => 0, :onlyif => :a_end?

        bit1 :c_end,     :initial_value => 0, :onlyif => :b_end?
        bit7 :c_size,    :initial_value => 0, :onlyif => :b_end?

        bit1 :d_end,     :initial_value => 0, :onlyif => :c_end?
        bit7 :d_size,    :initial_value => 0, :onlyif => :c_end?

        def a_end?
            a_end != 0
        end

        def b_end?
            b_end != 0
        end

        def c_end?
            c_end != 0
        end

        def to_i
            (d_size << (3 * 7)) | (c_size << (2 * 7)) | (b_size << 7) | a_size
        end

        def read_size
            base = 1
            base += 1 if b_end?
            base += 1 if c_end?
            base += 1 if d_end != 0
            base
        end
    end

    class DeltaOffset < DeltaSize
        def to_i
            rez = a_size 
            rez = ((rez + 1) << 7) + b_size if a_end?
            rez = ((rez + 1) << 7) + c_size if b_end?
            rez = ((rez + 1) << 7) + d_size if c_end?
            rez
        end
    end

    class Delta < BinData::Record
        bit1 :is_src,       :initial_value => 0
        bit7 :ini_size,     :initial_value => 0

        # if it's src, we must read it, parsing is a bit complicated
        uint8 :offset_1,    :initial_value => 0, :onlyif => :offset_byte1?
        uint8 :offset_2,    :initial_value => 0, :onlyif => :offset_byte2?
        uint8 :offset_3,    :initial_value => 0, :onlyif => :offset_byte3?
        uint8 :offset_4,    :initial_value => 0, :onlyif => :offset_byte4?

        uint8 :size_1,      :initial_value => 0, :onlyif => :size_byte1?
        uint8 :size_2,      :initial_value => 0, :onlyif => :size_byte2?
        uint8 :size_3,      :initial_value => 0, :onlyif => :size_byte3?

        # if just raw data, only read it, simple
        array :data, :type => :uint8, :initial_length => :ini_size, :onlyif => :is_raw?

        def data_from(file)
            return data if is_raw?

            file.seek(offset, IO::SEEK_SET)
            file.read(size)
        end

        def offset
            offset_1 | (offset_2 << 8) | (offset_3 << 16) | (offset_4 << 24)
        end

        def size
            sz = size_1 | (size_2 << 8) | (size_3 << 16)

            return 0x10000 if sz == 0
            sz
        end

        def size_byte1?
            is_src != 0 && ini_size & 16 != 0
        end

        def size_byte2?
            is_src != 0 && ini_size & 32 != 0
        end

        def size_byte3?
            is_src != 0 && ini_size & 64 != 0
        end

        def offset_byte1?
            is_src != 0 && ini_size & 1 != 0
        end

        def offset_byte2?
            is_src != 0 && ini_size & 2 != 0
        end

        def offset_byte3?
            is_src != 0 && ini_size & 4 != 0
        end

        def offset_byte4?
            is_src != 0 && ini_size & 8 != 0
        end

        def is_raw?
            is_src == 0
        end
    end

    class DeltaPack < BinData::Record
        # modif?
        delta_size  :src_size
        delta_size  :rez_size
        array       :deltas, :type => :delta
    end

    class PackFileEntryHeader < BinData::Record
        bit1 :a_end
        bit3 :type
        bit4 :a_size

        bit1 :b_end,     :initial_value => 0, :onlyif => :a_end?
        bit7 :b_size,    :initial_value => 0, :onlyif => :a_end?

        bit1 :c_end,     :initial_value => 0, :onlyif => :b_end?
        bit7 :c_size,    :initial_value => 0, :onlyif => :b_end?

        bit1 :d_end,     :initial_value => 0, :onlyif => :c_end?
        bit7 :d_size,    :initial_value => 0, :onlyif => :c_end?

        def read_size
            rez = 1
            rez += 1 if a_end?
            rez += 1 if b_end?
            rez += 1 if c_end?
            rez
        end

        def uncompressed_size
            (d_size << 18) | (c_size << 11) | (b_size << 4) | a_size 
        end

        def obj_type
            type
        end

        OBJ_COMMIT = 1
        OBJ_TREE   = 2
        OBJ_BLOB   = 3
        OBJ_TAG    = 4
        OBJ_OFS_DELTA = 6
        OBJ_REF_DELTA = 7

        def delta?
            type == OBJ_OFS_DELTA || type == OBJ_REF_DELTA
        end

        def a_end?
            a_end != 0
        end

        def b_end?
            b_end != 0
        end

        def c_end?
            c_end != 0
        end
    end

    class PackFileHeader < BinData::Record
        string    :pack_str, :read_length => 4
        uint32be  :pack_version
        uint32be  :entries_count
        uint8     :padding

        HEADER_SIZE = 4 + 4 + 4 + 1

        def valid?
            pp [pack_str, pack_version, entries_count, padding ]
            pack_str == "PACK" && pack_version == 2
        end
    end
end

