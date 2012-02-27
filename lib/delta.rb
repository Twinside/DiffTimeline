require 'bindata'

class String
  unless method_defined? :byteslice
    ##
    # Does the same thing as String#slice but
    # operates on bytes instead of characters.
    #
    def byteslice(*args)
      unpack('C*').slice(*args).pack('C*')
    end
  end
end

module GitRead
    class DeltaSrc
        attr_reader :offset, :size
        def initialize(offset, size)
            @offset = offset
            @size = size
        end

        def content(str)
            str.byteslice(@offset, @size)
        end
    end

    class DeltaCopy
        attr_reader :data
        def initialize(data)
            @data = data
        end

        def content(str)
            @data
        end
    end

    class RawDelta < BinData::Record
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
        string :data, :read_length => :ini_size, :onlyif => :is_raw?

        def to_delta
            if is_raw?
                DeltaCopy.new(data)
            else
                DeltaSrc.new(offset, size)
            end
        end
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

    class DeltaPack
        attr_reader :delta_cmd

        def initialize(delta_cmd)
            @delta_cmd = delta_cmd
        end

        def apply_delta(str)
            acc = ''
            @delta_cmd.each { |cmd| acc += cmd.content(str) }
            acc
        end
    end

    class RawDeltaPack < BinData::Record
        delta_size  :src_size
        delta_size  :rez_size
        array       :deltas, :type => :raw_delta, :read_until => :eof

        def to_delta
            DeltaPack.new(deltas.map { |v| v.to_delta })
        end
    end
end
