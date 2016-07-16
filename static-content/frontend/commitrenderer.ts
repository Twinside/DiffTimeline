/// <reference path="difftimeline.extern.ts" />
/// <reference path="resultset.ts" />
/// <reference path="diffmanipulator.ts" />
/// <reference path="project.ts" />
/// <reference path="jquery.d.ts" />
/// <reference path="global_constant.ts" />

var insert_node = function( node : Element | JQuery ) {  
    var view_mode = Project.state.active_view_mode();

    if (view_mode == Project.ViewMode.VIEW_FULL) {
        $('.container').prepend(node);
    }
    else if (view_mode == Project.ViewMode.VIEW_COMPACT) {
        $('.container').append(node);
    }
}

class KeyCommitAssoc
{
    [ix: string]: Commit;
};

class CommitRenderer implements ResultSet
{
    private current_mode: (msg: GuiMessage) => void;
    private keys: KeyCommitAssoc;
    private initial_view_mode: Project.ViewMode;
    private focused_index: number;
    private fetching: boolean;
    private collection: Commit[];
    private selection_index: number;
    private orig_node: JQuery;
    private result_list: any[];

    public constructor() {
        this.current_mode = this.normal_mode;
        this.keys = new KeyCommitAssoc(); 
        this.initial_view_mode = Project.state.active_view_mode();
        this.focused_index = 0;
        this.fetching = false;
    }

    private init(init_data : CommitDetail ) {
        var new_commit = new Commit(init_data.key, init_data);

        var new_node = new_commit.create_dom();
        insert_node(new_node);
        $(new_node).addClass(global_focus);
        new_commit.render();
        
        this.collection = [new_commit];
        this.keys[init_data.key] = new_commit;
    }

    private fetch_commit( id: Ref, f : (e: CommitDetail|ErrorReturn) => void ) {
        var this_obj = this;

        $.ajax({
            url: '/commit/' + id,
            dataType: 'json',
            data: {},
            success: f,
            error: function() {
                show_error({error: 'Communication error with the server while fetching commit'});
            }
        });
    }

    public render_all() {
        if (Project.state.active_view_mode() !== this.initial_view_mode) {
            this.initial_view_mode = Project.state.active_view_mode();
            $('.container *').remove();
            this.create_all_dom();
        }

        for ( var i = 0; i < this.collection.length; i++ ) {
            this.collection[i].render();
        }
    }

    private fetch_tree(key : Ref) {
        this.keys[key].fetch_tree();
    }

    private move_left() {
        $(this.collection[this.focused_index].orig_node).removeClass(global_focus);

        if (this.focused_index === this.collection.length - 1) {
            this.fetch_previous(0);
        } else {
            this.focused_index++;
            var new_focused_node = this.collection[this.focused_index].orig_node;
            $(new_focused_node).addClass(global_focus);
            $(document).scrollTo(new_focused_node, 200, {offset: Project.state.chrome_scroll_offset()});
        }
    }

    private move_right() {
        if (this.focused_index === 0) return;
        $(this.collection[this.focused_index].orig_node).removeClass(global_focus);
        this.focused_index--;
        var new_focused_node = this.collection[this.focused_index].orig_node;
        $(new_focused_node).addClass(global_focus);
        $(document).scrollTo(new_focused_node, 200, {offset: Project.state.chrome_scroll_offset()});
    }

    private clear_command() {
        var command = $('.command_line_file');
        var input = $('input', command);
        var form = $('form', command);

        input.blur();
        command.css('visibility', 'hidden');
        form.unbind('submit');
        input.unbind('keyup');

        this.current_mode = this.normal_mode;
    }

    private move_command_selection(offset : number) {
        var new_val = this.selection_index + offset;
        this.selection_index = Math.max(0, Math.min(new_val, this.result_list.length - 1));

        var results = $('.file_search_result', this.orig_node);
        results.removeClass('focused_commit');
        $(results[this.selection_index]).addClass('focused_commit');
    }

    private command_request() {
        var command = $('.command_line_file');
        var input = $('input', command);
        var form = $('form', command);
        var file_list = $('.match_list', command);

        $('> *', file_list).remove();
        command.css("visibility", "visible");

        this.result_list = [];
        this.selection_index = 0;
        var this_obj = this;
        var is_first = true;

        input.keyup(function(e) {
            if (is_first) {
                $(input).val('');
                is_first = false;
                return;
            }

            // detect the escape key
            if (e.keyCode === 27) {
                this_obj.clear_command();
                return true;
            } else if (e.keyCode === 38) { // up
                this_obj.move_command_selection(-1);
                return true;
            } else if (e.keyCode === 40) { // down
                this_obj.move_command_selection(1);
                return true;
            }

            var val = $(input).val();

            if (val[0] === ':')
                val = val.slice(1, val.length);

            val = new RegExp(val, 'i');

            var curr_commit = this_obj.collection[this_obj.collection.length - 1];
            curr_commit.find_matching_files(val, function(lst) {
                var maxi = Math.min(10, lst.length);
                this_obj.result_list = lst;
                this_obj.selection_index = 0;
                $('> *', file_list).remove();

                for ( var i = 0; i < maxi; i++ ) {
                    var new_node = ich.file_search_result(lst[i]);
                    if (i === this_obj.selection_index)
                        new_node.addClass('focused_commit');
                    make_draggable_elems(new_node);
                    file_list.append(new_node);
                }
            });
        });

        form.submit(function () {
            this_obj.clear_command();

            var elem = this_obj.result_list[this_obj.selection_index];
            Project.state.switch_file(elem.full_path, elem.hash, elem.key);

            this_obj.result_list = [];
            return false;
        });

        $(input).focus();
        return false;
    }

    private command_mode( msg : GuiMessage ) {
        if (msg.action === Project.GuiMessageCode.ESCAPE)
            this.clear_command();
        else if (msg.action === Project.GuiMessageCode.MOVE_UP)
            this.move_command_selection(-1);
        else if (msg.action === Project.GuiMessageCode.MOVE_DOWN)
            this.move_command_selection(1);
    }

    private normal_mode( msg : GuiMessage ) {
        if (msg.action === Project.GuiMessageCode.FETCH_TREE)
            this.fetch_tree((msg as { action: any; key: string }).key);
        else if (msg.action === Project.GuiMessageCode.MOVE_LEFT)
            this.move_left();
        else if (msg.action === Project.GuiMessageCode.MOVE_RIGHT)
            this.move_right();
        else if (msg.action === Project.GuiMessageCode.MOVE_FIRST) {
            this.focused_index = Math.max(0, this.collection.length - 2);
            this.move_left();
        }
        else if (msg.action === Project.GuiMessageCode.MOVE_LAST) {
            this.current_mode = this.command_mode;
            this.focused_index = 1;
            this.move_right();
        }
        else if (msg.action === Project.GuiMessageCode.COMMAND_REQUEST) {
            this.current_mode = this.command_mode;
            this.command_request();
        }
        else
            this.collection[this.focused_index].send_message(msg);
    }


    public send_message( msg : GuiMessage ) {
        this.current_mode(msg);
    }

    public create_all_dom() {
        for ( var i = 0; i < this.collection.length; i++ ) {
            insert_node(this.collection[i].create_dom());
            this.collection[i].post_insert();
        }
    }

    public fetch_previous(id: number) {
        var this_obj = this;
        var parents = this.collection[this.collection.length - 1].parents_sha;
        var prev_id = parents.length > 0 ? parents[0].key : null_ref;

        if (this_obj.fetching)
            return;

        this_obj.fetching = true;

        this.fetch_commit(prev_id, function(data) {
            if (data.hasOwnProperty('error')) {
                show_error( data as ErrorReturn);
                return;
            }

            var cd = data as CommitDetail;
            var new_commit = new Commit(cd.key, cd);

            var new_node = new_commit.create_dom();
            insert_node(new_node);
            new_node.animate({'width':'toggle'}, 0);
            new_commit.render();
            new_node.animate({'width':'toggle'}, Project.state.apparition_duration());

            this_obj.collection.push(new_commit);
            this_obj.keys[cd.key] = new_commit;
            this_obj.move_left();

            this_obj.fetching = false;
        });
    }

    public static create_from_data(init_data : CommitDetail) : CommitRenderer {
        var rez = new CommitRenderer();
        rez.init(init_data);
        return rez;
    }

    public static create_from_arg(key : Ref) : CommitRenderer {
        var rez = new CommitRenderer();

        rez.fetch_commit(key, function(data) { 
            if (data.hasOwnProperty('error')) {
                show_error(data as ErrorReturn);
                return undefined
            }
            rez.init(data as CommitDetail);
        });

        return rez;
    }

    public gui_descr = 
        { compact_view: true, fetch_previous: true
        , context_size: false, syntax_toggle: false };
}

