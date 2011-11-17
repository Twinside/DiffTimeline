require 'pp'

module GitRead
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

        def overlap?(e)
            @orig_idx <= e.dest_idx && e.dest_idx < @orig_idx + @size
        end

        def contain?(e)
            overlap?(e) && @orig_idx + @size >= e.dest_idx + e.size
        end

        def <(e)
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

        def each
            @listing.each { |v| yield v }
        end

        def to_json(*a)
            json = @listing.map { |v| v.to_json }.join( "\n, " )
            '[ ' + json + ' ]'
        end

        def each_add
            @listing.each { |c| yield c if c.cmd == :add_line  }
        end

        def each_rem
            &listing.each { |c| yield c if c.cmd == :rem_line }
        end

        class ChangeRange
            attr :beg, :last, :way
            def initialize(way, beg, last)
                @way = way
                @beg = beg
                @last = last
            end
        end

        # keep only :add from self, keep only ; del from other
        # ---------
        #              ------------ l'un puis l'autre
        #   akay
        #
        #   ------------ le début du premier, puis le second puis la find du dernier
        #       ----
        #
        #   ---------
        #        ----------
        #   le début du premier, puis le combiné, et enfin la fin du dernier
        def merge(other_diffset)
            ranges = []

            lefts = []
            other_diffset.each_rem do |c|
                lefts << ChangeRange.new(:rem, c.orig_idx, c.orig_idx + c.size)
            end

            rights = []
            adds_actions do |c|
                rights << ChangeRange.new(:add, c.dest_idx, c.dest_idx + c.size)
            end

            def combiner(a,b)
                if a == :add && b == :rem)
                    :addrem
                else
                    :remadd
                end
            end

            left_read_index = 0
            right_read_index = 0

            left = lefts[left_read_index]
            right = rights[right_read_index]

            def swapArrays
                var swap_idx = left_read_index
                left_read_index = right_read_index
                right_read_index = swap_idx

                var swap_array = lefts
                lefts = rights
                rights = swap_array

                var swap_obj = left
                left = right
                right = swap_obj
            end

            def inc_right
                right_read_index += 1
                right = rights[right_read_index] if right_read_index < rights.size
            end

            def inc_left
                left_read_index += 1
                left = lefts[left_read_index] if left_read_index < lefts.size
            end

            while left_read_index < lefts.size && right_read_index < rights.size do
                if right.beg >= right.end 
                    inc_right
                elsif left.beg >= left.end
                    inc_left
                elsif right.beg < left.beg 
                    swapArrays

                elsif left.beg < right.beg && left.end < right.beg
                    ranges << ChangeRange.new(left.way, left.beg, left.last)
                    inc_left

                elsif left.beg < right.beg && left.end <= right.end
                    ranges << ChangeRange.new(left.way, left.beg, right.beg - 1)
                    ranges << ChangeRange.new(combiner(left.way, right.way),
                                              right.beg, left.last)
                    right.beg = left.end + 1
                    inc_left
                elsif left.beg == right.beg
                    if left.end <= right.end
                        ranges << ChangeRange.new(combiner(left.way, right.way),
                                                    left.beg, left.last)
                        right.last = left.end + 1
                        inc_left
                    else
                        ranges << ChangeRange.new(combiner(left.way, right.way),
                                                  left.beg, right.last)
                        left.beg = right.end + 1
                        inc_right
                    end
                elsif left.beg < right.beg && left.end > right.end
                    ranges << ChangeRange.new(left.way, left.beg, right.beg - 1)
                    ranges << ChangeRange.new(combiner(left.way, right.way),
                                              right.beg, right.end)
                    left.beg = right.end + 1
                    inc_right
                end
            end

            while left_read_index < lefts.size
                ranges << lefts[left_read_index]
                left_read_index += 1
            end

            while right_read_index < rights.size
                ranges << rights[right_read_index]
                right_read_index += 1
            end

            ranges.each { |v| pp v }
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

        # | Parse lines of a fil defining a diff of the following form :
        #  "+:35:6"
        #  /- -- -----> size
        #  |    \_____ begin line
        #  |
        # diff action
        # actions are 
        #
        #  * '+' : diff add
        #  * '-' : diff deletion
        #  * '~' : diff deletion then add
        #  * '+' : diff add then deletion
    end
end

