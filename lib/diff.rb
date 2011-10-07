require 'pp'

module GitRead
    class Diff
        def initialize( orig, dest )
            @coeffs = CoeffTable.new(orig.size, dest.size)
            @orig = orig
            @dest = dest
        end

        def dump
            @coeffs.dump
        end

        def Diff.diffFiles( filename1, filename2 )
            f1 = open(filename1, 'rb') do |file|
                file.read().lines.to_a
            end

            f2 = open(filename2, 'rb') do |file|
                file.read().lines.to_a
            end

            return nil if f1 == nil || f2 == nil
            diff = Diff.new( f1, f2 )
            diff.compute_diff
            diff
        end

        def compute_diff
            puts "Mkay"
            @coeffs.generate do |i, j|
                l = @orig[i - 1]
                r = @dest[j - 1]
                if l == r
                    @coeffs[i - 1][j - 1] + 1
                else
                    left = @coeffs[i - 1][j]
                    down = @coeffs[i][j - 1]
                    if left > down
                        left
                    else
                        down
                    end
                end
            end
        end

        def print_diff
            print_diff_at(@orig.length, @dest.length)
        end

        def print_diff_at(i, j)
            if i > 0 && j > 0 && @orig[i - 1] == @dest[j - 1]

                print_diff_at(i - 1, j - 1)
                print ("(    )  " + @orig[i - 1])

            elsif j > 0 && (i == 0 || @coeffs[i][j-1] >= @coeffs[i-1][j])

                print_diff_at(i, j-1)
                print (("(%4d)+ " % i) + @dest[j - 1])

            elsif i > 0 && (j == 0 || @coeffs[i][j-1] < @coeffs[i-1][j])

                print_diff_at(i - 1, j)
                print (("(%4d)- " % i) + @orig[i - 1])

            else
                puts ""
            end
        end

        class CoeffTable
            def initialize(length1, length2)
                @data = Array.new(length1 + 1) { Array.new(length2 + 1, 0) }
                @orig_length = length1
                @dest_length = length2
            end

            def generate
                puts @orig_length
                puts @dest_length
                for line in 1 .. @orig_length
                    for column in 1 .. @dest_length
                        @data[line][column] = yield line,column
                    end
                end
            end

            def [](x)
                @data[x]
            end

            def dump
                @data.each do |line|
                    line.each { |v| print "%4d " % v }
                    print "\n"
                end
            end
        end
    end
end
