/**
 * @constructor
 * @implements {ResultSet}
 */
var FileRendererBase = function(init_data) {
    var init_file = new FileBlob(init_data);

    /** @type {Array.<FileBlob>} */
    this.collection = [];
    this.keys = {};
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
};

var fetch_file = function(file, commit, filekey, f) {
    var request = '/ask_parent/' + commit;

    if (file[0] == '/') request += file;
    else request += '/' + file;

    $.ajax({ url: request, dataType: 'json',
           error: function() {
                    show_error({error: 'Communication error with the server while fetching file'});
                },
         success: f });
};

FileRendererBase.prototype.insert_node = function(node) {
    $(".container").prepend( node );
};

FileRendererBase.prototype.create_all_dom = function() {
    for ( var i = this.collection.length - 1; i >= 0; i-- ) {
        this.insert_node(this.collection[i].create_dom());
    }
};

FileRendererBase.prototype.render_all = function() {
    var i;

    for ( i = 0; i < this.collection.length - 1; i++ ) {
        this.collection[i].render(this.collection[i + 1].diff);
    }

    this.collection[i].render([]);
};

FileRendererBase.prototype.fetch_details = function(commit_id) {
    this.keys[commit_id].fetch_details();
};

FileRendererBase.prototype.move_left = function() {
    $(this.collection[this.focused_index].orig_node).removeClass(global_focus);

    if (this.focused_index === 0) {
        this.fetch_previous(0);
        return;
    }

    this.focused_index--;

    var new_focused_node = this.collection[this.focused_index].orig_node;
    $(new_focused_node).addClass(global_focus);
    this.collection[this.focused_index].focus_line(200);
};

FileRendererBase.prototype.move_right = function() {
    if (this.focused_index === this.collection.length - 1)
        return;

    $(this.collection[this.focused_index].orig_node).removeClass(global_focus);
    this.focused_index++;

    var new_focused_node = this.collection[this.focused_index].orig_node;
    $(new_focused_node).addClass(global_focus);
    this.collection[this.focused_index].focus_line(200);
};

FileRendererBase.prototype.synchronize_lines = function( targetted_line ) {
    var i;
    var computed_line;
    var curr_index = this.focused_index;
    var max_idx = this.collection.length;
    var matching_lines = new Array( max_idx );

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

    var blob;
    for (i = 0; i < max_idx; i++) {
        blob = this.collection[i];
        blob.set_line_offset(max_line - matching_lines[i]);
        blob.set_line(matching_lines[i]);
    }

    this.collection[ curr_index ].focus_line(5);
};

FileRendererBase.prototype.move_line_up = function() {
    var curr = this.collection[this.focused_index];
    this.synchronize_lines(curr.move_line_up());
};

FileRendererBase.prototype.move_line_down = function() {
    var curr = this.collection[this.focused_index];
    this.synchronize_lines(curr.move_line_down());
};

FileRendererBase.prototype.send_message = function( msg ) {
    if (msg.action === Project.GuiMessage.FETCH_DETAIL)
        return this.fetch_details(msg.key);
    else if (msg.action === Project.GuiMessage.MOVE_LEFT)
        return this.move_left();
    else if (msg.action === Project.GuiMessage.MOVE_RIGHT)
        return this.move_right();
    else if (msg.action === Project.GuiMessage.MOVE_DOWN)
        return this.move_line_down();
    else if (msg.action === Project.GuiMessage.MOVE_UP)
        return this.move_line_up();
    else if (msg.action === Project.GuiMessage.COMMAND_REQUEST) {
        var this_obj = this;

        var abs = function (line) {
            var curr = this_obj.collection[this_obj.focused_index];
            this_obj.synchronize_lines(curr.set_line(line));
        };

        var rel = function (offset) {
            var curr = this_obj.collection[this_obj.focused_index];
            this_obj.synchronize_lines(curr.offset_line(offset));
        };

        return this.aligner.command_request(abs, rel);
    }
    else if (msg.action === Project.GuiMessage.SELECT_AS_LEFT)
        this.collection[this.focused_index].select_as_left();
    else if (msg.action === Project.GuiMessage.SELECT_AS_RIGHT)
        this.collection[this.focused_index].select_as_right();
    else if (msg.action === Project.GuiMessage.SWITCH_BLAME)
        this.collection[this.focused_index].switch_blame();
};

FileRendererBase.prototype.fetch_previous = function(id) {
    var last_commit = this.collection[0];
    var this_obj = this;

    if (this.fetching) return;

    if (last_commit.parent_commit.length <= 0) {
        show_error( "The commit has no parents" );
        return;
    }

    var to_fetch = last_commit.parent_commit[id].key;

    this_obj.fetching = true;

    fetch_file(last_commit.file, to_fetch,
                last_commit.filekey, function(data) {
                        
        if (data === null) {
            show_error({error: 'Communication error with the server'});
            return;
        }

        if (data['error']) { 
            show_error( data );
            return;
        }

        var new_commit = new FileBlob(data);

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
};

FileRendererBase.prototype.gui_descr =
    { compact_view: true, fetch_previous: true
    , context_size: true, syntax_toggle: false };


var FileRenderer = {
    create_from_data: function(init_data) { 
        return new FileRendererBase(init_data);
    },

    create_from_arg: function(file, filekey, commit, callback) {
        fetch_file(file, commit, filekey, function(data) { 
            data.file = file;
            callback( new FileRendererBase(data) );
        });
    }
};

