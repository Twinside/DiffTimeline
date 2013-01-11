/**
 * @constructor
 * @implements {ResultSet}
 */
var FileComparer = function(key1, file1, key2, file2) {
    this.key1 = key1;
    this.file1 = file1;
    this.key2 = key2;
    this.file2 = file2;
    this.refresh_diff();
    this.aligner = new FileAlign();
    this.line_index = 0;
}

FileComparer.prototype.fetch_previous = function(id) {
    show_error({error: 'Does not exists in this mode'});
};

FileComparer.prototype.render_all = function() {
    var rems = DiffManipulator.toDiffDelRange( DiffManipulator.filterRems(this.data.diff) );
    var adds = DiffManipulator.toDiffAddRange( DiffManipulator.filterAdds(this.data.diff) );
    var number_node = $('.line_number_column');
    var node = $('.syntax_highlighted');

    number_node.children().remove();

    if (Project.state.active_view_mode() === Project.ViewMode.VIEW_COMPACT) {
        DiffManipulator.generateCompactHtml(this.file1, Project.state.active_context_size(),
                                            false, this.data.data_orig, rems,
                                            number_node[0], node[0]);

        DiffManipulator.generateCompactHtml(this.file2, Project.state.active_context_size(),
                                            false, this.data.data_dest, adds,
                                            number_node[1], node[1]);
    }
    else { // render full
        DiffManipulator.generateFullHtml(this.file1, false, this.data.data_orig, rems,
                                            number_node[0], node[0]);

        DiffManipulator.generateFullHtml(this.file2, false, this.data.data_dest, adds,
                                            number_node[1], node[1]);
    }
};

FileComparer.prototype.refresh_diff = function() {
    var this_obj = this;
    var url = ('/compare_files/' + encodeURIComponent(this.key1)
                            + '/' + encodeURIComponent(this.file1.slice(1))
                            + '/' + encodeURIComponent(this.key2)
                            + this.file2);

    $.ajax({  
        url: url,
        dataType: 'json',
        data: {},
        error: function() {
            show_error({error: 'Communication error with server while comparing files'}); },
        success: function(data) {
            if (data['error']) { 
                show_error( data );
                return;
            }

            this_obj.data = data;
            this_obj.create_all_dom();
            this_obj.render_all();
        }
    });
}

FileComparer.prototype.align_abs = function(side_id, line) {
    var this_obj = this;

    var translaters = [
        function (line) { return DiffManipulator.toNextDiff(line, this_obj.data.diff); },
        function (line) { return DiffManipulator.toPrevDiff(line, this_obj.data.diff); }
    ];

    var padders = $('.container .file_content .align_padder pre');

    var other_id = 1 - side_id;
    var next_line = (translaters[side_id])(line);
    var line_diff = Math.abs(line - next_line);

    
    if (next_line >= line) {
        $(padders[side_id]).text(this.aligner.create_padding(line_diff));
        $(padders[other_id]).text('');
    } else {
        $(padders[side_id]).text('');
        $(padders[other_id]).text(this.aligner.create_padding(line_diff));
    }

    var columns = $('.line_number_column');
    var tmp = $('.syntax_line_number', columns[side_id]);
    var number = tmp[line];
    $(document).scrollTo(number, 20,
                         {offset: Project.state.chrome_scroll_offset()});

    $('.highlighted_line', columns).removeClass('highlighted_line');
    $(number).addClass('highlighted_line');
    $($('.syntax_line_number', columns[other_id])[next_line]).addClass('highlighted_line');
};

FileComparer.prototype.create_all_dom = function() {
    var created = ich.compare_files(this);
    $('.container').append(created);

    var this_obj = this;

    this.aligner.make_number_clickable(created, function(side_id, line) {
        this_obj.align_abs(side_id, line);
    });
};

FileComparer.prototype.move_line_down = function() {
    this.line_index = FileAlign.move_line_down(this.line_index, $('.line_number_column')[0]);
    this.align_abs(0, this.line_index);
    return this.line_index;
};

FileComparer.prototype.move_line_up = function() {
    this.line_index = FileAlign.move_line_up(this.line_index, $('.line_number_column')[0]);
    this.align_abs(0, this.line_index);
    return this.line_index;
};

FileComparer.prototype.send_message = function( msg ) {

    if (msg.action === Project.GuiMessage.MOVE_DOWN)
        return this.move_line_down();
    else if (msg.action === Project.GuiMessage.MOVE_UP)
        return this.move_line_up();
    if (msg.action === Project.GuiMessage.COMMAND_REQUEST) {
        var this_obj = this;

        var abs = function (line) {
            this_obj.align_abs(0, line);
            this_obj.line_index = line;
        };

        var rel = function (offset) {
            var line = this_obj.line_index + offset;
            this_obj.align_abs(0, this_obj.line_index + offset);
            this_obj.line_index = line;
        };

        return this.aligner.command_request(abs, rel);
    }
};

FileComparer.prototype.gui_descr =
        { compact_view: true, fetch_previous: false
        , context_size: true, syntax_toggle: false };

FileComparer.create_from_args = function(key1, file1, key2, file2) {
    return new FileComparer(key1, file1, key2, file2);
};

