/**
 * @param {ParentFile} data
 * @constructor
 */
var FileBlob = function (data) {
    "use strict";

    /** @type {function(string, Array.<string>) : Array.<Element>} */
    var add_syntax_coloration = function( filename, lines )
    {
        /** @type {Array.<Element>} */
        var ret = [];
        var highlighter = TinySyntaxHighlighter.from_filename(true, filename);

        /** @type {number} */
        var i;

        for ( i = 0; i < lines.length; i++ )
            ret.push(highlighter.colorLine(lines[i]));

        return ret;
    }

    /** @type {boolean} */
    this.detail_fetched = false;

    /** @type {string} */
    this.file = data.filename;

    this.binary = data.binary;
    this.diff = data.diff;
    this.data = data.data;

    this.commit_date = timestamp_to_string(data.timestamp);

    /** @type {ref} */
    this.filekey = data.filekey;

    /** @type {ref} */
    this.key = data.key;

    /** @type {string} */
    this.message = data.message;
    this.parent_commit = data.parent_commit;
    this.path = data.path;

    var tooltips = [];
    for (var i = 0; i < this.parent_commit.length; i++) {
        tooltips.push( this.parent_commit[i].key + " - " + this.parent_commit[i].message );
    }
    Project.state.set_previous_button_count(this.parent_commit.length, tooltips);

    for (var i = 0; i < this.path.length; i++) {
        this.path[i].commit_date = timestamp_to_string(this.path[i].timestamp);

        this.path[i].splited_message =
            html_encodize(this.path[i].message).replace("\n", "<br/>");
    }

    this.ellipsis_size = this.path.length - 15;


    /** @type {number} */
    this.line_index = 0;

    return this;
};

FileBlob.prototype.set_line = function(line) {
    var numbers = $('.syntax_line_number', this.orig_node);

    if (line < 0)
        line = 0;
    if (line >= numbers.length)
        line = numbers.length - 1;

    this.line_index = line;

    $('.highlighted_line', this.orig_node).removeClass('highlighted_line');
    $(numbers[this.line_index]).addClass('highlighted_line');

    return line;
};

FileBlob.prototype.set_line_offset = function( offset ) {
    var node = $('.align_padder pre', this.orig_node);

    if (offset === 0) {
        node.html('');
        return;
    }

    var str_offset = '\n ';

    for (var i = 1; i < offset; i++)
        str_offset += ' \n ';

    node.html(str_offset);
};

FileBlob.prototype.compute_matching_line_from_past = function( line ) {
    return DiffManipulator.toNextDiff(line, this.diff);
};

FileBlob.prototype.compute_matching_line_from_future = function( line ) {
    return DiffManipulator.toPrevDiff(line, this.diff);
};

FileBlob.prototype.create_dom_details = function(node) {
    var detail_node = $('.commit_detail', node);

    for ( var i = 0; i < this.details.length; i++ )
    {
        var e = this.details[i];
        e.key = this.key;
        e.full_path = e.name;
        detail_node.append(ich.commit_file(e));
    }

    make_draggable_elems(node);
};

FileBlob.prototype.toggle_detail_pane = function(node) {
    var detail = $('.commit_detail', node);
    var btn_node = $('.more_info', node);
    var btn_text = btn_node.text();

    if (btn_text[0] == '▼')
        btn_node.html("&#x25b2");
    else
        btn_node.html("&#x25bc");

    detail.animate({height: 'toggle'}, Project.state.apparition_duration);
};

FileBlob.prototype.fetch_details = function() {
    if (this.detail_fetched)
    {
        this.toggle_detail_pane(this.orig_node);
        return;
    }

    var this_obj = this;
    $.ajax({
        url: '/ask_commit/' + this.key,
        dataType: 'json',
        data: {},
        error: function() {
            show_error({error: 'Communication error with the server while fetching commit details'});
        },
        success: 
        
            /** @param {ParentFile|ErrorReturn} data */
            function(data) {
            if (data === null) {
                show_error({error: 'Communication error with the server'});
                return;
            }

            if (data['error']) { 
                show_error( data );
                return;
            }

            this_obj.detail_fetched = true;

            var kind_formater = {
                'modification': ich.commit_file_modification,
                'addition':ich.commit_file,
                'deletion':ich.commit_file
            };

            this_obj.details = data;

            var detail_node = $('.commit_detail', this_obj.orig_node);
            detail_node.animate({height: 'toggle'}, 0);
            this_obj.create_dom_details(this_obj.orig_node);
            this_obj.toggle_detail_pane(this_obj.orig_node);
        }
    });
};

FileBlob.prototype.render_file_data = function(next_diff, number_node, node) {
    var filename = this.file;
    var diff     = this.diff;
    var data     = this.data;

    var rems = DiffManipulator.filterRems(next_diff);
    var adds = DiffManipulator.filterAdds(diff);
    var ranges = DiffManipulator.calculateFoldSet(rems, adds);

    var clean_cr_lf_data = data.replace(/\r/g, '');

    if (!this.binary) {
        if (Project.state.active_view_mode() === Project.ViewMode.VIEW_COMPACT)
            DiffManipulator.generateCompactHtml(filename, Project.state.active_context_size(),
                                                false, clean_cr_lf_data, ranges,
                                                number_node[0], node[0]);
        else // render full
            DiffManipulator.generateFullHtml(filename, false, clean_cr_lf_data, ranges,
                                                number_node[0], node[0]);
    } else {
        node[0].appendChild(document.createTextNode("Binary element"));
    }
};

FileBlob.prototype.create_dom = function() {
    this.short_message = this.message.split("\n")[0];
    this.splited_message = html_encodize(this.message).replace("\n", "<br/>");

    var path_length = this.path.length;
    var maximum_path_length = 15;

    if (path_length > maximum_path_length)
    {
        this.path_beg = this.path.slice(0, maximum_path_length / 2);
        this.path_end = this.path.slice(this.path.length - maximum_path_length / 2,
                                        this.path.length - 1);
        this.orig_node = ich.commitfile_huge_path(this);
    }
    else
        this.orig_node = ich.commitfile(this);

    if (this.detail_fetched)
    {
        this.create_dom_details(this.orig_node);
        this.toggle_detail_pane(this.orig_node);
        $('.more_info', this.orig_node).html("&#x25bc");
    }

    make_draggable_elems(this.orig_node);
    return this.orig_node;
};

FileBlob.prototype.render = function(next_diff) {
    var render_node = $('.syntax_highlighted', this.orig_node);
    var number_node = $('.line_number_column', this.orig_node);

    render_node.detach();
    render_node.empty();
    number_node.empty();
    this.render_file_data(next_diff, number_node, render_node);
    render_node.appendTo($('table td:last', this.orig_node));
};

FileBlob.prototype.offset_line = function(offset) {
    return this.set_line(this.line_index + offset);
};

FileBlob.prototype.move_line_up = function () {
    this.line_index = FileAlign.move_line_up(this.line_index, this.orig_node);
    return this.line_index;
};

FileBlob.prototype.move_line_down = function() {
    this.line_index = FileAlign.move_line_down(this.line_index, this.orig_node);
    return this.line_index;
}

/** @param {number} speed */
FileBlob.prototype.focus_line = function(speed) {
    if (this.line_index === 0) {
        $(document).scrollTo(this.orig_node, speed,
                    {offset: Project.state.chrome_scroll_offset()});
        return;
    }

    var numbers = $('.syntax_line_number', this.orig_node);
    var offsets = Project.state.chrome_scroll_offset();
    offsets.left -= 30;
    offsets.top -= 200;

    $(document).scrollTo(numbers[this.line_index], speed, {offset: offsets });
};

