/// <reference path="resultset.ts" />
/// <reference path="linealign.ts" />
/// <reference path="diffmanipulator.ts" />
/// <reference path="difftimeline.extern.ts" />
/// <reference path="resultset.ts" />
/// <reference path="project.ts" />
/// <reference path="fileblob.ts" />

class KeyAssocs
{
    [ix: string] : FileBlob;
};

class FileRenderer implements ResultSet
{
    private collection : FileBlob[];
    private keys : KeyAssocs;
    private focused_index : number;
    private fetching : boolean;
    private aligner : FileAlign;

    public constructor(init_data : ParentFile) {
        var init_file = new FileBlob(init_data);

        this.collection = [];
        this.keys = new KeyAssocs();
        this.focused_index = 0;
        this.fetching = false;

        this.collection = [init_file];
        this.keys[init_file.key] = init_file;

        var new_node = init_file.create_dom();
        this.insert_node( new_node );
        this.aligner = new FileAlign();
        $(new_node).addClass(global_focus);
        init_file.render([]);

        return this;
    }

    private static fetch_file(file: string,
                              commit : Ref,
                              filekey : any,
                              f: (data: ParentFile | ErrorReturn) => void) {
        var request = '/ask_parent/' + commit;

        if (file[0] == '/')
            file = file.substring(1);
        
        request += '/' + encodeURIComponent(file);

        $.ajax({ url: request, dataType: 'json',
               error: function() {
                   show_error({error: 'Communication error with the server while fetching file'});
               },
               success: f });
    }

    public insert_node(node: Element | JQuery) {
        $(".container").prepend( node );
    }

    public create_all_dom() {
        for ( var i = this.collection.length - 1; i >= 0; i-- ) {
            this.insert_node(this.collection[i].create_dom());
        }
    }

    public render_all() {
        for ( var i = 0; i < this.collection.length - 1; i++ ) {
            this.collection[i].render(this.collection[i + 1].diff);
        }

        this.collection[i].render([]);
    }

    public fetch_details(commit_id : Ref) {
        this.keys[commit_id].fetch_details();
    }

    public move_left() {
        $(this.collection[this.focused_index].orig_node).removeClass(global_focus);

        if (this.focused_index === 0) {
            this.fetch_previous(0);
            return;
        }

        this.focused_index--;

        var new_focused_node = this.collection[this.focused_index].orig_node;
        $(new_focused_node).addClass(global_focus);
        this.collection[this.focused_index].focus_line(200);
    }

    public move_right() {
        if (this.focused_index === this.collection.length - 1)
            return;

        $(this.collection[this.focused_index].orig_node).removeClass(global_focus);
        this.focused_index++;

        var new_focused_node = this.collection[this.focused_index].orig_node;
        $(new_focused_node).addClass(global_focus);
        this.collection[this.focused_index].focus_line(200);
    }

    public synchronize_lines( targetted_line : number ) {
        var i : number;
        var curr_index = this.focused_index;
        var max_idx = this.collection.length;
        var matching_lines : number[] = new Array( max_idx );

        matching_lines[curr_index] = targetted_line;

        for (i = curr_index; i < max_idx - 1; i++) {
            matching_lines[i + 1] =
                this.collection[i + 1].compute_matching_line_from_past(matching_lines[i]);
        }

        for (i = curr_index; i > 0; i--) {
            matching_lines[i - 1] =
                this.collection[i].compute_matching_line_from_future(matching_lines[i]);
        }

        var max_line = Math.max.apply(Math, matching_lines);

        for (i = 0; i < max_idx; i++) {
            let blob = this.collection[i];
            blob.set_line_offset(max_line - matching_lines[i]);
            blob.set_line(matching_lines[i]);
        }

        this.collection[ curr_index ].focus_line(5);
    }

    public move_line_up() {
        var curr = this.collection[this.focused_index];
        this.synchronize_lines(curr.move_line_up());
    }

    public move_line_down() {
        var curr = this.collection[this.focused_index];
        this.synchronize_lines(curr.move_line_down());
    }

    public send_message( msg : GuiMessage ) : any {
        if (msg.action === Project.GuiMessageCode.FETCH_DETAIL)
            return this.fetch_details((msg as any).key);
        else if (msg.action === Project.GuiMessageCode.MOVE_LEFT)
            return this.move_left();
        else if (msg.action === Project.GuiMessageCode.MOVE_RIGHT)
            return this.move_right();
        else if (msg.action === Project.GuiMessageCode.MOVE_DOWN)
            return this.move_line_down();
        else if (msg.action === Project.GuiMessageCode.MOVE_UP)
            return this.move_line_up();
        else if (msg.action === Project.GuiMessageCode.COMMAND_REQUEST) {
            var this_obj = this;

            var abs = function (line : number) {
                var curr = this_obj.collection[this_obj.focused_index];
                this_obj.synchronize_lines(curr.set_line(line));
            };

            var rel = function (offset : number) {
                var curr = this_obj.collection[this_obj.focused_index];
                this_obj.synchronize_lines(curr.offset_line(offset));
            };

            return this.aligner.command_request(abs, rel);
        }
        else if (msg.action === Project.GuiMessageCode.SELECT_AS_LEFT)
            this.collection[this.focused_index].select_as_left();
        else if (msg.action === Project.GuiMessageCode.SELECT_AS_RIGHT)
            this.collection[this.focused_index].select_as_right();
        else if (msg.action === Project.GuiMessageCode.SWITCH_BLAME)
            this.collection[this.focused_index].switch_blame();
    }

    public fetch_previous(id : number) {
        var last_commit = this.collection[0];
        var this_obj = this;

        if (this.fetching) return;

        if (last_commit.parent_commit.length <= 0) {
            show_error( {error: "The commit has no parents"});
            return;
        }

        var to_fetch = last_commit.parent_commit[id].key;

        this_obj.fetching = true;

        FileRenderer.fetch_file(last_commit.file, to_fetch,
                                last_commit.filekey, function(data) {
                            
            if (data === null) {
                show_error({error: 'Communication error with the server'});
                return;
            }

            if (data.hasOwnProperty('error')) { 
                show_error( data as ErrorReturn );
                return;
            }

            var new_commit = new FileBlob(data as ParentFile);

            var node = new_commit.create_dom();
            this_obj.insert_node(node);

            this_obj.collection.unshift( new_commit );
            this_obj.focused_index++;
            this_obj.keys[new_commit.key] = new_commit;
            node.animate({'width': 'toggle'}, 0);
            this_obj.collection[0].render(this_obj.collection[1].diff);
            node.animate({'width': 'toggle'}, Project.state.apparition_duration() * 2);
            this_obj.move_left();

            this_obj.fetching = false;
        });
    }


    public static create_from_data(init_data : ParentFile) { 
        return new FileRenderer(init_data);
    }

    public static create_from_arg(file : string,
                                  filekey : any,
                                  commit : Ref,
                                  callback : (fr : FileRenderer) => void) {
        FileRenderer.fetch_file(file, commit, filekey, function(data : ParentFile) { 
            data.file = file;
            callback( new FileRenderer(data) );
        });
    }

    public gui_descr =
        { compact_view: true, fetch_previous: true
        , context_size: true, syntax_toggle: false };
}

