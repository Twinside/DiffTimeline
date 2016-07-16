/// <reference path="resultset.ts" />
/// <reference path="linealign.ts" />
/// <reference path="diffmanipulator.ts" />
/// <reference path="difftimeline.extern.ts" />

class BlameShower implements ResultSet {
    private data: BlameInfo;
    private line_index: number;
    private aligner: FileAlign;
    private orig_node: JQuery;

    public constructor(data : BlameInfo) {
        this.data = data;
        this.aligner = new FileAlign();
        this.line_index = 0;

        const ranges = this.data.ranges;

        for (let i = 0; i < ranges.length; i++) {
            ranges[i].padd_string = this.split_cut_lines(ranges[i]);
        }

        this.create_all_dom();
        this.render_all();
    }

    public split_cut_lines(range : BlameRangeSource) : string {
        const n = range.size - 1;
        let ret = '';
        const messageLines = range.tag.message.split('\n');
        let i = 0;

        const splitedMessageLines : string[] = [];
        for (i = 0; i < messageLines.length; i++) {
            const sub = messageLines[i].match(/.{1,30}/g);

            if (!sub) continue;
            if (sub.length > 1) {
                for (var j = 0; j < sub.length; j++) {
                    splitedMessageLines.push(sub[j] + " ...");
                }
            }
            else splitedMessageLines.push(sub[0]);
        }

        if (n > 0) {
            ret += '\n(' + range.tag.author + ')';
        }

        for (i = 1; i - 1 < splitedMessageLines.length && i < n; i++) {
            ret += '\n' + splitedMessageLines[i - 1];
        }

        for (i = i; i < n; i++) ret += '\n&nbsp;';
        return ret;
    }

    public set_backgrounds_colors() {
        const ranges = this.data.ranges;

        var nodes = $('.blame_range', this.orig_node);
        var date_interval = this.data.latest - this.data.earliest;

        for (var i = 0; i < ranges.length; i++) {
            var range = ranges[i];
            var zeroToOne = (range.tag.timestamp - this.data.earliest) / date_interval;
            var newColor = color_lerp(blame_gradient.beg, blame_gradient.end, zeroToOne);

            $(nodes[i]).css('background-color', newColor);
            $(nodes[i]).hover(function() {
                $(this).addClass('blame_hover');
            }, function() {
                $(this).removeClass('blame_hover');
            });
        }
    }

    public fetch_previous(id : any) {
        show_error({error: 'Does not exists in this mode'});
    }

    public render_all() {
        const render_node = $('.syntax_highlighted', this.orig_node);
        const number_node = $('.line_number_column', this.orig_node);
        const clean_cr_lf_data = this.data.data.replace(/\r/g, '');

        DiffManipulator.generateFullHtml(this.data.filename, false,
                                            clean_cr_lf_data, [],
                                            number_node[0], render_node[0]);
        this.set_backgrounds_colors();
    }

    public create_all_dom() {
        this.orig_node = ich.blamefile(this.data);
        $('.container').append(this.orig_node);
    }

    public align_abs(line : number) {
        const numbers = $('.line_number_column .syntax_line_number');
        const number = numbers[line];
        const offset = Project.state.chrome_scroll_offset();
        offset.top -= 30;
        offset.left -= 200;
        $(document).scrollTo(number, 20,
                             {offset: offset});

        $('.highlighted_line', numbers).removeClass('highlighted_line');
        $(number).addClass('highlighted_line');
    }

    public move_line_down() {
        this.line_index = FileAlign.move_line_down(this.line_index, $('.line_number_column')[0]);
        this.align_abs(this.line_index);
        return this.line_index;
    }

    public move_line_up() {
        this.line_index = FileAlign.move_line_up(this.line_index, $('.line_number_column')[0]);
        this.align_abs(this.line_index);
        return this.line_index;
    }

    public check_line(line : number) : number {
        if (line < 0) return 0;

        var numbers = $('.syntax_line_number');
        if (line >= numbers.length)
            return numbers.length - 1;

        return line;
    }

    public send_message( msg : GuiMessage ) : number | boolean {
        if (msg.action === Project.GuiMessageCode.MOVE_DOWN)
            return this.move_line_down();
        else if (msg.action === Project.GuiMessageCode.MOVE_UP)
            return this.move_line_up();
        if (msg.action === Project.GuiMessageCode.COMMAND_REQUEST) {
            var this_obj = this;

            var abs = function (line : number) {
                this_obj.line_index = this_obj.check_line(line);
                this_obj.align_abs(this_obj.line_index);
            };

            var rel = function (offset : number) {
                var line = this_obj.check_line(this_obj.line_index + offset);
                this_obj.align_abs(line);
                this_obj.line_index = line;
            };

            return this.aligner.command_request(abs, rel);
        }
    };

    public static create_from_data(data : BlameInfo) {
        return new BlameShower(data);
    }

    public static create_from_arg(key : Ref,
                                  file : string,
                                  f: (bs: BlameShower) => void) {
        if (file[0] == '/') 
            file = file.substring(1);

        $.ajax({ url: '/blame/' + encodeURIComponent(key) + '/' + encodeURIComponent(file),
            dataType: 'json',
            data: {},
            error: function() {
                show_error({error: 'Communication error with the server while fetching blame'});
            },
            success: function(data) {
                if (data['error']) { 
                    show_error( data );
                    return;
                }

                f(new BlameShower(data));
            }
        });
    }

    public gui_descr =
        { compact_view: false, fetch_previous: false
        , context_size: false, syntax_toggle: false }
}

