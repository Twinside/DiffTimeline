/** @type {!Object.<Project.DiffKind, function(json) : !jQuery>} */
var kind_formater = {};



class KindAssoc {
    public constructor() {
        this[Project.DiffKind.KIND_DELETION] = ich.commit_file;
        this[Project.DiffKind.KIND_ADDITION] = ich.commit_file;
        this[Project.DiffKind.KIND_MODIFICATION] = renderCommit;
    }
    [ix: string]: (json: JSON) => JQuery;
};

var kind_format : KindAssoc;

$(document).ready(function() {
    kind_format = new KindAssoc();
});

function renderCommit(e : JSON) : JQuery {
    var hl = TinySyntaxHighlighter.from_filename(false, e.name);
    var rez_node : JQuery = ich.commit_file_modification_detailed(e);

    /** @type {Element} */
    var code_node : Element  = rez_node.find('.syntax_highlighted')[0];

    /** @type {Element} */
    var number_node = rez_node.find('.line_number_column')[0];

    var curr_diff;
    var acc = ""
    var diff_node;
    var prev_end = -1;

    var create_number_node = function(n) {
        var node = document.createElement('span');
        node.setAttribute('class', 'syntax_line_number');
        node.appendChild(document.createTextNode(n.toString() + "\n"));
        return node;
    };

	/** @type {function(string) : Element} */
    var div_node = function(kind) {
        var node = document.createElement('div');
        node.setAttribute('class', kind);
        return node;
    };

    if (e.binary) {
        return rez_node;
    }

    for ( var i = 0; i < e.diff.length; i++ )
    {
        var has_sub = false;

        curr_diff = e.diff[i];
		has_sub = (curr_diff.hasOwnProperty('sub') &&
				   curr_diff.sub.length > 0);

        if (curr_diff.way == '+') {
            diff_node = div_node("diff_addition");
            hl.set_current_line_number(curr_diff.dest_idx + 1);
        }
        else if (curr_diff.way == '-') {
            diff_node = div_node("diff_deletion");
            hl.set_current_line_number(curr_diff.dest_idx + 1);
        }
        else {
            diff_node = div_node("diff_context");
            hl.set_current_line_number(curr_diff.dest_idx + 1);

            if (prev_end != curr_diff.dest_idx)
                hl.reset_context();
        }

        prev_end = curr_diff.dest_idx + curr_diff.data.length;

        for ( var l = 0; l < curr_diff.data.length; l++ )
        {
            if (has_sub) {
                hl.setPositionHighlight(curr_diff.sub[l]);
            }

            var lineNodes = hl.colorLine(curr_diff.data[l] + "\n");
            if (has_sub) {
                hl.setPositionHighlight([]);
            }

            number_node.appendChild(create_number_node(hl.current_line - 1));

            for ( var node = 0; node < lineNodes.length; node++ )
                diff_node.appendChild(lineNodes[node]);
        }

        code_node.appendChild(diff_node);

        if (i < e.diff.length - 1 &&
            curr_diff.dest_idx + curr_diff.size < e.diff[i + 1].dest_idx )
        {
            number_node.appendChild(document.createTextNode("...\n"));
            code_node.appendChild(document.createTextNode("...\n"));
        }

    }

    return rez_node;
};

////////////////////////////////////////////////////////////////////
//// Real Commit
////////////////////////////////////////////////////////////////////
class Commit {
    private key : ref;
    private is_key_real: boolean;
    private commit_date: string;
    private parents_sha: {message: string, key: ref}[];
    private head_message : string;
    private sub_message : string;
    private split_message: string;
    private tree_fetched : boolean;
    private tree_opened : boolean;
    private focused_diff : number;
    private fully_fetched: boolean;

    private message: string;

    private tree: JSON;
    private author: any;
    private file_changes : any;
    private diff_nodes : any[];

    private last_view_mode : Project.ViewMode;
    private orig_node : Element;

    public constructor( key: ref, data : /*commitdetail*/ JSON ) {
        this.key = key === null_ref ? display_null_ref : key;
        this.is_key_real = key !== null_ref;
        this.commit_date = timestamp_to_string(data['timestamp']);
        this.parents_sha = data['parents_sha'];
        this.file_changes = data['file_changes'];
        this.author = data['author'];
        this.message = data['message'];

        var i : number;
        var tooltips : string[] = [];
        for (i = 0; i < this.parents_sha.length; i++)
        {
            var overview = this.parents_sha[i];
            tooltips.push(overview.key + " : " + overview.message);
        }

        Project.state.set_previous_button_count(this.parents_sha.length, tooltips);

        var messages_lines = data['message'].split('\n');
        var first_non_null = 0;

        while (messages_lines[first_non_null] === '')
            { first_non_null++; }
        
        this.head_message =
            html_encodize(messages_lines[first_non_null]);

        this.sub_message =
            html_encodize(messages_lines.slice(first_non_null + 1,
                                                messages_lines.length).join('\n')).replace(/\n/g, '<br/>');
        
        this.split_message =
            (html_encodize(data['message'])).replace(/\n/g, '<br/>');
        this.tree_fetched = false;
        this.tree_opened = false;
        this.last_view_mode = Project.state.active_view_mode();
        this.focused_diff = 0;
        this.diff_nodes = [];

        if (data.hasOwnProperty('file_changes')) {
            this.fully_fetched = true;
            this.file_changes = data['file_changes'];
        } else {
            this.fully_fetched = false;
            this.file_changes = [];
        }
    }
    
    public move_up() {
        if (this.focused_diff === 0) {
            $(document).scrollTo(this.orig_node, 300, {offset: Project.state.chrome_scroll_offset()});
            return;
        }

        $(this.diff_nodes[this.focused_diff]).removeClass(sub_focus);

        this.focused_diff--;
        var new_node = this.diff_nodes[this.focused_diff];
        $(new_node).addClass(sub_focus);
        $(document).scrollTo(new_node, 300, {offset: Project.state.chrome_scroll_offset()});
    }

    public move_down() {
        if ( this.focused_diff === this.diff_nodes.length - 1 )
            return;

        $(this.diff_nodes[this.focused_diff]).removeClass(sub_focus);

        this.focused_diff++;
        var new_node = this.diff_nodes[this.focused_diff];
        $(new_node).addClass(sub_focus);
        $(document).scrollTo(new_node, 300, {offset: Project.state.chrome_scroll_offset()});
    }

    public send_message(msg : GuiMessage) {
        if (msg.action === Project.GuiMessage.MOVE_UP)
            return this.move_up();
        else if (msg.action === Project.GuiMessage.MOVE_DOWN)
            return this.move_down();
        else if (msg.action === Project.GuiMessage.MOVE_INNER) {
            var file = this.file_changes[this.focused_diff];
            return Project.state.switch_file(file.name, file.hash, file.key);
        }
    }

    public render_tree(node : Element, depth : number, tree_path : string, 
                       elem /* CommitTreeDiff */) : Element {
        var new_node : Element;
        var opened = false;

        elem.key = this.key;
        elem.full_path = (depth > 0) ? tree_path + "/" + elem.name
                                    : elem.name;

        if (elem.hasOwnProperty('children') && elem.children.length > 0)
        {
            if (depth == 0) {
                for ( var i = 0; i < elem.children.length; i++ )
                    this.render_tree(node, depth + 1, 
                                    elem.full_path, elem.children[i]);
                return node;
            }

            new_node = ich.tree_folder(elem);
            var child_node = $(".children", new_node);
            var button_indicator = $(".button", new_node);
            node.appendChild(new_node[0]);

            for ( var i = 0; i < elem.children.length; i++ )
                this.render_tree(child_node[0], depth + 1, 
                                elem.full_path, elem.children[i]);

            child_node.animate({height: 'toggle'}, 0);

            $('> div.node_name', new_node).click(function( event ){
                event.stopPropagation();
                child_node.animate({height: 'toggle'}, 400);

                if (opened)
                    button_indicator.html('&#x25bc');
                else
                    button_indicator.html('&#x25b2');

                opened = !opened;
            });

        } else {
            new_node = ich.tree_elem(elem);
            make_draggable_elems( new_node );
            node.appendChild(new_node[0]);
        }

        return new_node;
    }

    public fetch_tree_raw(callback : () => void) {
        var tree_node = $(".commit_tree", this.orig_node);
        var indicator = $(".btn_indicator", this.orig_node);

        if (this.tree_fetched) {
            indicator.html(this.tree_opened ? "&#x25bc;" : "&#x25b2;");
            tree_node.animate({height: 'toggle'});
            this.tree_opened = !this.tree_opened;
            return;
        }

        var this_obj = this;

        $.ajax({ url: '/ask_commit_tree/' + this.key,
            dataType: 'json',
            data: {},
            error: function() {
                show_error({error: 'Communication error with the server while fetching commit tree'});
            },
            success: (data /* (CommitTreeDiff|ErrorReturn) */ ) => {

                if (data['error']) { 
                    show_error( data );
                    return;
                }
                tree_node.animate({height: 'toggle'});
                this_obj.tree = data;
                this_obj.tree_fetched = true;
                remove_children(tree_node[0]);
                this_obj.render_tree(tree_node[0], 0, "", data);
                callback();
            }
        });
    }

    public fetch_tree() {
        var this_obj = this;

        this.fetch_tree_raw(function() {
            this_obj.tree_opened = true;

            var tree_node = $(".commit_tree", this.orig_node);
            tree_node.animate({height: 'toggle'});

            var indicator = $(".btn_indicator", this.orig_node);
            indicator.html("&#x25b2;");
        });
    }

    public find_matching_file_list(pattern : RegExp) : string[] {
        var ret_list = [];
        var recurse = (elem) => {
            if (elem.hasOwnProperty('children') && elem.children.length > 0)
            {
                for ( var i = 0; i < elem.children.length; i++ )
                    recurse(elem.children[i]);
            } else {
                if ( pattern.test(elem.full_path) )
                    ret_list.push(elem);
            }
        }

        recurse(this.tree);

        return ret_list;
    }

    public find_matching_files(pattern: RegExp, callback : (matches : string[]) => void) {
        if (!this.tree_fetched) {
            var this_obj = this;
            this.fetch_tree_raw(function (){
                callback( this_obj.find_matching_file_list(pattern) );
            });
        }
        else
            callback( this.find_matching_file_list(pattern) );
    }

    public post_insert() {
        var view_mode = Project.state.active_view_mode();

        if (view_mode == Project.ViewMode.VIEW_FULL && this.tree_fetched) {
            var tree_node = $(".commit_tree", this.orig_node);
            this.render_tree(tree_node[0], 0, "", this.tree);
            this.tree_opened = false;
            tree_node.animate({height: 'toggle'});
        }
    }

    public create_dom() : Element {
        var view_mode = Project.state.active_view_mode();
        this.tree_opened = false;

        if (view_mode == Project.ViewMode.VIEW_FULL) {
            this.orig_node = ich.commit_detailed(this);
        }
        else if (view_mode == Project.ViewMode.VIEW_COMPACT) {
            this.orig_node = ich.commit_compact(this);
        }

        return this.orig_node;
    }

    public render_full() {
        this.diff_nodes = [];
        this.focused_diff = 0;
        var table = $('.commit_content', this.orig_node);

        for ( var change in this.file_changes ) {
            var e = this.file_changes[change];
            var kind = e['kind'];
            e.key = this.key;
            /* displayed path is full path */
            e.full_path = e.name;

            var new_node = ( kind_formater.hasOwnProperty(kind)
                        ? kind_formater[kind](e)
                        : ich.commit_file(e) );

            table.append( new_node );
            this.diff_nodes.push( new_node );
        }

        make_draggable_elems( this.orig_node );
        $(this.diff_nodes[this.focused_diff]).addClass(sub_focus);
    };

    public render_compact() {
        this.diff_nodes = [];
    };

    public render() {
        var view_mode = Project.state.active_view_mode();

        if (view_mode == Project.ViewMode.VIEW_FULL) {
            this.render_full();
        } else if (view_mode == Project.ViewMode.VIEW_COMPACT) {
            this.render_compact();
        }
    }
}

    