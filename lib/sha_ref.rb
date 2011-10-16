module GitRead
    class ShaRef
        def initialize(shaInfo)
            if shaInfo.class == String
                @sha = shaInfo.chomp
                @raw_array = transform_to_array(@sha)
            else # assume array like
                @sha = stringify(shaInfo)
                @raw_array = shaInfo
            end
        end

        def loose_path
            @sha.clone.insert(2, '/')
        end

        def ==(other)
            @raw_array == other.raw
        end

        def to_s
            @sha
        end

        def raw
            @raw_array
        end

        def is_greater_than(arr)
            i = 0
            while i < 20
                if @raw_array[i] < arr[i]
                    return false
                elsif @raw_array[i] > arr[i]
                    return true
                end
                i += 1
            end

            false
        end

    private
        def stringify(arr)
            "%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x" % arr
        end

        def transform_to_array(txt_sha)
            20.times.map { |i| txt_sha[2*i..2*i+1].hex }.to_a
        end
    end
end

