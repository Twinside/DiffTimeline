module GitRead
    class Diff
        def initialize( orig, dest )
            @coeffs = CoeffTable.new(orig.size, dest.size)
            @orig = orig
            @dest = dest
        end

    private
        def compute_diff
            @coeffs.generate do |line, col|
                if line == 0 || col == 0
                    return 0
                end

                if @orig[i] == @dest[j]
                    return @coeffs[i - 1][j - 1] + 1
                else
                    left = @coeffs[i - 1][j]
                    down = @coeffs[i][j - 1]
                    return left if left > down
                    return down
                end
            end
        end

        class CoeffTable
            def initialize(length1, length2)
                @data = Array.new(length1 + 1) { Array.new(length2 + 1) }
                @orig_length = length1
                @dest_length = length2

                # set the first line to 0
                @data[0][0..length2] = 0
                # set the first column to 0
                @data[0..length 1][0] = 0
            end

            def generate
                for line in (0 .. @orig_length - 1)
                    for column in (0 .. @dest_length - 1)
                        @data[line][column] = yield line,column
                    end
                end
            end

            def [](x, y)
                @data[x][y]
            end

            def []=(x, y, value)
                @data[x][y] = value
            end
        end
    end
end
