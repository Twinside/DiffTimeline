module GitRead
    class ShaRef
        def initialize(shaInfo)
            if shaInfo.class == Array
                @sha = stringify(shaInfo)
                @raw_array = shaInfo
            else
                @sha = shaInfo.chomp
                @raw_array = transform_to_array(@sha)
            end
        end

        def loose_path
            @sha.insert(2, '/')
        end

        def raw
            @raw_array
        end

        def is_greater_than(arr)
            i = 0
            while i < 20
                if @raw_array[i] < arr[i]
                    return false
                elseif @raw_array[i] > arr[i]
                    return true
                end
                i += 1
            end

            false
        end

    private
        def stringify(arr)
            "%2X%2X%2X%2X%2X%2X%2X%2X%2X%2X%2X%2X%2X%2X%2X%2X%2X%2X%2X%2X" % arr
        end

        def transform_to_array(txt_sha)
            20.times.map { |i| txt_sha[2*i..2*i+1].hex }.to_a
        end
    end
end

