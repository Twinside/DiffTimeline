
module GitRead
    class Diff
        attr_reader :diff_set

        # Class describin a diff action to pass from
        # origin to destination.
        class DiffCommand
            attr_reader :cmd, :orig_idx, :dest_idx, :size

            # (:add_line | :rem_line, Int, Int) -> DiffCommand
            def initialize(cmd, idx, ddx, size)
                @cmd = cmd
                @size = size
                @orig_idx = idx
                @dest_idx = ddx
            end

            def inc_size(s)
                @size += s
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

        def print_diff
            @diff_set.each do |d|
                case d.cmd
                when :rem_line
                    (d.orig_idx .. d.orig_idx + d.size - 1).each do |i| 
                        puts "- (#{i}) #{@orig[i]}"
                    end

                when :add_line
                    (d.dest_idx .. d.dest_idx + d.size - 1).each do |i|
                        puts "+ (#{i}) #{@dest[i]}"
                    end
                end
            end
        end

        class DiffSet
            def initialize()
                @listing = []
            end

            def each
                @listing.each { |v| yield v }
            end

            def to_json(*a)
                json = @listing.map { |v| v.to_json }.join( "\n, " )
                '[ ' + json + ' ]'
            end

            def add_range(orig_line, dest_line, size)
                if @listing.size == 0
                    @listing << DiffCommand.new(:add_line, orig_line, dest_line, size)
                else
                    prev = @listing.last

                    if prev.cmd == :add_line && dest_line == prev.dest_idx + prev.size
                        @listing.last.inc_size(size)
                    else
                        @listing << DiffCommand.new(:add_line, orig_line, dest_line, size)
                    end
                end
            end

            def rem_range(orig_line, dest_line, size)
                if @listing.size == 0
                    @listing << DiffCommand.new(:rem_line, orig_line, dest_line, size)
                    return
                else
                    prev = @listing.last

                    if prev.cmd == :rem_line && orig_line == prev.orig_idx + prev.size
                        @listing.last.inc_size(size)
                    else
                        @listing << DiffCommand.new(:rem_line, orig_line, dest_line, size)
                    end
                end
            end
        end

        # ([String], [String]) -> Diff
        # you should probably use diffFiles
        def initialize( orig, dest )
            @orig = orig
            @dest = dest

            @orig_hash = Array.new(@orig.size) { |i| @orig[i].hash }
            @dest_hash = Array.new(@dest.size) { |i| @dest[i].hash }

            coef_size = (@orig.size + @dest.size) * 2
            @forward_snake = Array.new(coef_size, -1)
            @backward_snake = Array.new(coef_size, -1)

            @diff_set = DiffSet.new
            compute_diff
        end

        def compute_diff
            lcs(0, @orig_hash.size, 0, @dest_hash.size)
        end

        def lcs(orig_begin, orig_end, dest_begin, dest_end)

            while orig_begin < orig_end && dest_begin < dest_end && @orig_hash[orig_begin] == @dest_hash[dest_begin]
                orig_begin += 1
                dest_begin += 1
            end

            while orig_begin < orig_end && dest_begin < dest_end && @orig_hash[orig_end - 1] == @dest_hash[dest_end - 1]
                orig_end -= 1
                dest_end -= 1
            end

            # we found a snake
            if orig_begin == orig_end
                @diff_set.add_range(orig_begin, dest_begin, dest_end - dest_begin)
                return
            elsif dest_begin == dest_end
                @diff_set.rem_range(orig_begin, dest_begin, orig_end - orig_begin)
                return
            else
                snake_range = find_middle_snake(orig_begin, orig_end, dest_begin, dest_end)

                lcs(orig_begin, snake_range.x, dest_begin, snake_range.y)
                lcs(snake_range.x, orig_end, snake_range.y, dest_end)
            end
        end

        def find_max_reaching_snake(x,y, max_x, max_y)
            while x < max_x && y < max_y && @orig_hash[x] == @dest_hash[y]
                x += 1
                y += 1
            end

            x
        end

        def find_max_reaching_snake_backward(x,y, x_min, y_min)
            while x_min < x && y_min < y && @orig_hash[x - 1] == @dest_hash[y - 1]
                x -= 1
                y -= 1
            end

            x
        end

        class DiffPoint
            attr_accessor :x, :y

            def initialize(x,y)
                @x = x
                @y = y
            end
        end

        class Range
            attr_accessor :min, :max
            def initialize(mini, maxi)
                @min = mini
                @max = maxi
            end

            # (Range, Vector, Int) -> ()
            def increase_within(bounds, vector, null_value)
                if @min > bounds.min
                    @min -= 1
                    vector[@min - 1] = null_value
                else
                    @min += 1
                end

                if @max < bounds.max
                    @max += 1
                    vector[ @max + 1 ] = null_value
                else
                    @max -= 1
                end
            end

            def range_backward
                d = @max
                while d >= @min
                    yield d
                    d -= 2
                end
            end

            def contain(x)
                @min <= x && x <= @max
            end
        end

        def find_middle_snake(x_min, x_max, y_min, y_max)
            valid_diagonal = Range.new(x_min - y_max, x_max - y_min)

            forward_mid = x_min - y_min
            backward_mid = x_max - y_max

            forward_range = Range.new(forward_mid, forward_mid)
            backward_range = Range.new(backward_mid, backward_mid)

            @forward_snake[forward_mid] = x_min
            @backward_snake[backward_mid] = x_max

            is_odd = (forward_mid - backward_mid).odd?

            while true
                forward_range.increase_within(valid_diagonal, @forward_snake, -1)
                forward_range.range_backward do |d|
                    lower_diag = @forward_snake[d - 1]
                    upper_diag = @forward_snake[d + 1]
                    # we choose the biggest diagonal, the one reaching the
                    # furthest
                    x = upper_diag
                    x = lower_diag + 1 if lower_diag >= upper_diag

                    snake = find_max_reaching_snake(x, x - d, x_max, y_max)
                    @forward_snake[d] = snake

                    if is_odd && backward_range.contain(d) && @backward_snake[d] <= snake
                        return DiffPoint.new(x, x - d)
                    end
                end

                backward_range.increase_within(valid_diagonal, @backward_snake, 9999999)
                backward_range.range_backward do |d|
                    lower_diag = @backward_snake[d - 1]
                    # we choose the smallest diagonal, the one reaching the furthest.
                    upper_diag = @backward_snake[d + 1]
                    x = lower_diag
                    x = upper_diag - 1 if lower_diag >= upper_diag

                    snake = find_max_reaching_snake_backward(x, x - d, x_min, y_min)
                    @backward_snake[d] = snake

                    if !is_odd && forward_range.contain(d) && snake <= @forward_snake[d]
                        return DiffPoint.new(x, x - d)
                    end
                end
            end
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
            Diff.new( f1, f2 )
        end

        # (String, String) -> Diff
        def Diff.diff_strings( file_content_a, file_content_b )
            lines_a = file_content_a.lines.to_a
            lines_b = file_content_b.lines.to_a
            Diff.new( lines_a, lines_b )
        end
    end
end

