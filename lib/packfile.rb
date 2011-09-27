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
        array    :fanout, :type => :uint32be, :initial_length => 254
        uint32be :compcount
        array    :shas, :type => :bin_sha,   :initial_length => :compcount
        array    :crcs, :type => :uint32be, :initial_length => :compcount
        array    :offsets, :type => :uint32be, :initial_length => :compcount
        bin_sha  :packChecksum
        bin_sha  :indexChecksum
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

        def a_end?
            a_end != 0
        end

        def b_end?
            b_end != 0
        end

        def c_end?
            c_end != 0
        end

        def uncompressed_size
            (d_size << 18) | (c_size << 11) | (b_size << 4) | a_size 
        end
        # next line is wrong, it is the size of the expended data.
        # array :data, :type => :uint8, :initial_length => lambda {}
    end

    class PackFileHeader < BinData::Record
        string    :pack_str, :read_length => 4
        uint32be  :pack_version
        uint32be  :entries_count
        uint8     :padding
    end

    def GitRead.test
        pp "Meuh"
    end
end

