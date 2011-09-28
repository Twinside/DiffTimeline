module GitRead
    class ShaRef
        def initialize(shaInfo)
            if shaInfo.class == Array
                @sha = stringify(shaInfo)
                @raw_array = shaInfo
            else
                @sha = shaInfo
                @raw_array = transform_to_array(shaInfo)
            end
        end

        def loose_path
            @sha.insert(2, '/')
        end

        def raw
            @raw_array
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

