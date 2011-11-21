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
            @listing.each { |c| yield c if c.cmd == :rem_line }
        end

        class ChangeRange
            attr_accessor :beg, :last, :way
            def initialize(way, beg, last)
                @way = way
                @beg = beg
                @last = last
            end

            def to_s
                way_str = ''
                case @way
                when :addrem
                    way_str = '!'
                when :remadd
                    way_str = '~'
                when :add
                    way_str = '+'
                when :rem
                    way_str = '-'
                end

                "#{way_str}:#{@beg}:#{@last - @beg + 1}"
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
        def merge_with(other_diffset)
            ranges = []

            lefts = []
            other_diffset.each_rem do |c|
                lefts << ChangeRange.new(:rem, c.orig_idx, c.orig_idx + c.size)
            end

            rights = []
            each_add do |c|
                rights << ChangeRange.new(:add, c.dest_idx, c.dest_idx + c.size)
            end

            def combiner(a,b)
                if a == :add && b == :rem
                    :addrem
                else
                    :remadd
                end
            end

            left_read_index = 0
            right_read_index = 0

            left = lefts[left_read_index]
            right = rights[right_read_index]

            swap_arrays = lambda do
                swap_idx = left_read_index
                left_read_index = right_read_index
                right_read_index = swap_idx

                swap_array = lefts
                lefts = rights
                rights = swap_array

                swap_obj = left
                left = right
                right = swap_obj
            end

            inc_right = lambda do
                right_read_index += 1
                right = rights[right_read_index] if right_read_index < rights.size
            end

            inc_left = lambda do
                left_read_index += 1
                left = lefts[left_read_index] if left_read_index < lefts.size
            end

            while left_read_index < lefts.size && right_read_index < rights.size do
                if right.beg >= right.last
                    inc_right.call()
                elsif left.beg >= left.last
                    inc_left.call()
                elsif right.beg < left.beg 
                    swap_arrays.call()

                elsif left.beg < right.beg && left.last < right.beg
                    yield ChangeRange.new(left.way, left.beg, left.last)
                    inc_left.call()

                elsif left.beg < right.beg && left.last <= right.last
                    yield ChangeRange.new(left.way, left.beg, right.beg - 1)
                    yield ChangeRange.new(combiner(left.way, right.way),
                                              right.beg, left.last)
                    right.beg = left.last + 1
                    inc_left.call()
                elsif left.beg == right.beg
                    if left.last <= right.last
                        yield ChangeRange.new(combiner(left.way, right.way),
                                                    left.beg, left.last)
                        right.last = left.last + 1
                        inc_left.call()
                    else
                        yield ChangeRange.new(combiner(left.way, right.way),
                                                  left.beg, right.last)
                        left.beg = right.last + 1
                        inc_right.call()
                    end
                elsif left.beg < right.beg && left.last > right.last
                    yield ChangeRange.new(left.way, left.beg, right.beg - 1)
                    yield ChangeRange.new(combiner(left.way, right.way),
                                          right.beg, right.last)
                    left.beg = right.last + 1
                    inc_right.call()
                end
            end

            while left_read_index < lefts.size
                yield lefts[left_read_index]
                left_read_index += 1
            end

            while right_read_index < rights.size
                yield rights[right_read_index]
                right_read_index += 1
            end
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
end

