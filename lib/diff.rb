require 'pp'

module GitRead
    class Diff
        # Class describin a diff action to pass from
        # origin to destination.
        class DiffCommand
            attr_reader :cmd, :orig_idx, :dest_idx, :size

            # (:add_line | :rem_line, Int, Int) -> DiffCommand
            def initialize(cmd, idx, ddx)
                @cmd = cmd
                @size = 1
                @orig_idx = idx
                @dest_idx = ddx
            end

            def inc_size
                @size += 1
            end

            def to_json(*a)
                ret = '{ "way": '
                case @cmd
                when :rem_line
                    ret += '"-", '
                when :add_line
                    ret += '"+", '
                end

                ret + "\"size\": #{@size}, \"orig_idx\":#{@orig_idx}, \"dest_idx\": #{@dest_idx}}"
            end
        end

        class DiffSet
            def initialize()
                @listing = []
            end

            def to_json(*a)
                json = @listing.map { |v| v.to_json }.join( "\n, " )
                '[ ' + json + ' ]'
            end

            def add_line(orig_line, dest_line)
                if @listing.size == 0
                    @listing << DiffCommand.new(:add_line, orig_line, dest_line)
                else
                    prev = @listing.last

                    if prev.cmd == :add_line && dest_line == prev.dest_idx + prev.size
                        @listing.last.inc_size
                    else
                        @listing << DiffCommand.new(:add_line, orig_line, dest_line)
                    end
                end
            end

            def rem_line(orig_line, dest_line)
                if @listing.size == 0
                    @listing << DiffCommand.new(:rem_line, orig_line, dest_line)
                    return
                else
                    prev = @listing.last

                    if prev.cmd == :rem_line && orig_line == prev.orig_idx + prev.size
                        @listing.last.inc_size
                    else
                        @listing << DiffCommand.new(:rem_line, orig_line, dest_line)
                    end
                end
            end
        end

        # ([String], [String]) -> Diff
        # you should probably use diffFiles
        def initialize( orig, dest )
            @orig = orig
            @dest = dest

            calculate_identical_offsets()

            @coeffs = Array.new(@orig_effective_size + 1) { Array.new(@dest_effective_size + 1, 0) }
        end

        # We try to reduce the diff to perform by finding
        # the identical beginning & end
        def calculate_identical_offsets
            i = 0
            while i < @orig.size && @dest.size
                if @orig[i] != @dest[i]
                    break
                end
                i += 1
            end
            @begin_offset = i

            orig_end = @orig.size - 1
            dest_end = @dest.size - 1

            while orig_end > 0 && dest_end > 0 && orig_end >= @begin_offset
                if @orig[orig_end] != @dest[dest_end]
                    break
                end
                orig_end -= 1
                dest_end -= 1
            end

            @orig_effective_size = orig_end - @begin_offset + 1
            @dest_effective_size = dest_end - @begin_offset + 1

            if @orig_effective_size > 0
                @orig_hash = Array.new(@orig_effective_size) { |i| @orig[i + @begin_offset].hash }
                @dest_hash = Array.new(@dest_effective_size) { |i| @dest[i + @begin_offset].hash }
            end
        end

        # nil
        # Print the coefficient table on stdout
        def dump
            @coeffs.dump
        end

        # (Filepath, Filepath) -> Diff
        # Helper static method to create a diff from
        # two filenames
        def Diff.diff_files( filename1, filename2 )
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

        # (String, String) -> Diff
        def Diff.diff_strings( file_content_a, file_content_b )
            lines_a = file_content_a.lines.to_a
            lines_b = file_content_b.lines.to_a
            diff = Diff.new( lines_a, lines_b )
            diff.compute_diff
            diff
        end

        # nil
        # Compute the coefficient matrix.
        def compute_diff
            for i in 1 .. @orig_effective_size
                for j in 1 .. @dest_effective_size

                    full_i = i + @begin_offset
                    full_j = j + @begin_offset

                    l = @orig_hash[i - 1]
                    r = @dest_hash[j - 1]
                    # Identical, so the matching score is
                    # the matching score of previous string +1
                    if l == r
                        @coeffs[i][j] = @coeffs[i - 1][j - 1] + 1
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
                        @coeffs[i][j] = [left, down].max
                    end
                end
            end
        end

        # DiffSet
        def diff_set
            if @orig_effective_size <= 0
                return DiffSet.new
            end

            acc = DiffSet.new
            diff_set_at(@orig_effective_size, @dest_effective_size, acc)
            acc
        end

        # (Int, Int, [DiffCommand]) -> nil
        def diff_set_at(i,j, rez)
            real_i = @begin_offset + i
            real_j = @begin_offset + j
            l = @orig_hash[i - 1]
            r = @dest_hash[j - 1]

            # We're not at the edge of the matrix, and data
            # is identical, so we're matching, no diff information
            # here.
            if i > 0 && j > 0 && l == r
                diff_set_at(i - 1, j - 1, rez)
            # i == 0 => no more lines in origin, so must add everything
            # else.
            # coeffs i,(j-1) >= coeffs (i-1),j =>
            # if we delete a line from the destination we get a better
            # matching score (in comparison of deleting a line from origin),
            # so declare an addition for the diff
            elsif j > 0 && (i == 0 || @coeffs[i][j-1] >= @coeffs[i-1][j])
                diff_set_at(i, j - 1, rez)
                rez.add_line( real_i - 1, real_j - 1)
            # inversly to previous case we get a better score by removing
            # a line, so follow this diff
            elsif i > 0 && (j == 0 || @coeffs[i][j-1] < @coeffs[i-1][j])
                diff_set_at(i - 1, j, rez)
                rez.rem_line(real_i - 1, real_j - 1)
            end
        end

        # nil
        # print the diff on stdout.
        def print_diff(only_diff)
            if !only_diff
                (0..(@begin_offset - 1)).each { |v| print "(BEGI)  #{@orig[v]}" }
            end

            print_diff_at(@orig_effective_size, @dest_effective_size, only_diff)

            if !only_diff
                ((@begin_offset + @orig_effective_size)..(@orig.size - 1)).each do |v|
                    puts "(END )  #{@orig[v]}"
                end
            end
        end

        # print the diff on stdout.
        # (Int, Int) -> nil
        def print_diff_at(i, j, only_diff)
            real_i = @begin_offset + i
            real_j = @begin_offset + j
            l = @orig[real_i - 1]
            r = @dest[real_j - 1]

            if i > 0 && j > 0 && l == r

                print_diff_at(i - 1, j - 1, only_diff)
                print ("(    )  " + l) if !only_diff

            elsif j > 0 && (i == 0 || @coeffs[i][j-1] >= @coeffs[i-1][j])

                print_diff_at(i, j-1, only_diff)
                print (("(%4d)+ " % real_i) + r)

            elsif i > 0 && (j == 0 || @coeffs[i][j-1] < @coeffs[i-1][j])

                print_diff_at(i - 1, j, only_diff)
                print (("(%4d)- " % real_i) + l)

            else
                puts ""
            end
        end
    end
end
