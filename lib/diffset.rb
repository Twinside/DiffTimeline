
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
            self_idx = 0
            rig_idx = 0


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

