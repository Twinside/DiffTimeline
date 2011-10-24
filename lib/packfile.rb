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
                return nil
            end
            middle = beg + (ending - beg) / 2

            if shas[middle].bits == sha.raw
                return offsets[middle]
            end

            if sha.is_greater_than(shas[middle].bits)
                return find_between(sha, middle + 1, ending)
            else
                return find_between(sha, beg, middle - 1)
            end
        end
    end

    class PackFileEntryHeader < BinData::Record
        bit1 :a_end
        bit3 :entry_type
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
            entry_type
        end

        OBJ_COMMIT = 1
        OBJ_TREE   = 2
        OBJ_BLOB   = 3
        OBJ_TAG    = 4
        OBJ_OFS_DELTA = 6
        OBJ_REF_DELTA = 7

        def delta?
            entry_type == OBJ_OFS_DELTA || entry_type == OBJ_REF_DELTA
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
            pack_str == "PACK" && pack_version == 2
        end
    end
end

