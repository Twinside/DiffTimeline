module GitRead
    class ShaRef
        def initialize(shaInfo)
            if shaInfo.class == Array
                @sha = stringify(shaInfo)
            else
                @sha = shaInfo
            end
        end

        def stringify(arr)
            "%2X%2X%2X%2X%2X%2X%2X%2X%2X%2X%2X%2X%2X%2X%2X%2X%2X%2X%2X%2X" % arr
        end
    end
end

