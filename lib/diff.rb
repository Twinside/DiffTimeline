require 'pp'

module GitRead
    class Diff
        # Class describin a diff action to pass from
        # origin to destination.
        class DiffCommand
            attr_reader :cmd, :orig_idx, :dest_idx

            # (:add_line | :rem_line, Int, Int) -> DiffCommand
            def initialize(cmd, idx, ddx)
                @cmd = cmd
                @orig_idx = idx
                @dest_idx = ddx
            end
        end

        # ([String], [String]) -> Diff
        # you should probably use diffFiles
        def initialize( orig, dest )
            @coeffs = CoeffTable.new(orig.size, dest.size)
            @orig = orig
            @dest = dest
        end

        # nil
        # Print the coefficient table on stdout
        def dump
            @coeffs.dump
        end

        # (String, String) -> Diff
        # Helper static method to create a diff from
        # two filenames
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

        # nil
        # Compute the coefficient matrix.
        def compute_diff
            @coeffs.generate do |i, j|
                l = @orig[i - 1]
                r = @dest[j - 1]
                # Identical, so the matching score is
                # the matching score of previous string +1
                if l == r
                    @coeffs[i - 1][j - 1] + 1
                else
                    #          ^
                    #          | down
                    #          |
                    # left +---+--+
                    #   <--+      |
                    #      +------+

                    # correspond to a score with a deletion
                    # of the origin
                    down = @coeffs[i - 1][j] 

                    # correspond to a score with an addition
                    # to the origin
                    left = @coeffs[i][j - 1]

                    # otherwise, it's the maximum score
                    # of the previously calculated data.
                    if down < left
                        down
                    else
                        left
                    end
                end
            end
        end

        # [DiffCommand]
        def diff_set
            acc = []
            diff_set_at(@orig.length, @dest.length, acc)
            acc
        end

        # (Int, Int, [DiffCommand]) -> nil
        def diff_set_at(i,j, rez)
            # We're not at the edge of the matrix, and data
            # is identical, so we're matching, no diff information
            # here.
            if i > 0 && j > 0 && @orig[i - 1] == @dest[j - 1]
                diff_set_at(i - 1, j - 1, rez)
            # i == 0 => no more lines in origin, so must add everything
            # else.
            # coeffs i,(j-1) >= coeffs (i-1),j =>
            # if we delete a line from the destination we get a better
            # matching score (in comparison of deleting a line from origin),
            # so declare an addition for the diff
            elsif j > 0 && (i == 0 || @coeffs[i][j-1] >= @coeffs[i-1][j])
                diff_set_at(i, j-1, rez)
                rez << DiffCommand.new(:add_line, i - 1, j - 1)
            # inversly to previous case we get a better score by removing
            # a line, so follow this diff
            elsif i > 0 && (j == 0 || @coeffs[i][j-1] < @coeffs[i-1][j])
                diff_set_at(i - 1, j, rez)
                rez << DiffCommand.new(:rem_line, i - 1, j - 1)
            end
        end

        # nil
        # print the diff on stdout.
        def print_diff
            print_diff_at(@orig.length, @dest.length)
        end

        # print the diff on stdout.
        # (Int, Int) -> nil
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

        # Coefficient matrix used in the Diff algorithm.
        class CoeffTable
            def initialize(length1, length2)
                @data = Array.new(length1 + 1) { Array.new(length2 + 1, 0) }
                @orig_length = length1
                @dest_length = length2
            end

            # nil
            # yield Int,Int -> Int
            # fill the coefficient table with information given
            # by the coroutine.
            def generate
                puts @orig_length
                puts @dest_length
                for line in 1 .. @orig_length
                    for column in 1 .. @dest_length
                        @data[line][column] = yield line,column
                    end
                end
            end

            # Int -> [Int]
            # Return the line at the given index.
            def [](x)
                @data[x]
            end

            # nil
            # Dump the coefficient matrix on stdout.
            def dump
                @data.each do |line|
                    line.each { |v| print "%4d " % v }
                    print "\n"
                end
            end
        end
    end
end
