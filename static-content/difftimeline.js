/////////////////////////////////////////////////////////////////////
//              Initial state
/////////////////////////////////////////////////////////////////////
var application_state = {  
    view_mode: 'full',
    apply_syntax_coloration: true,
    context_size: 2,
    max_commit_delta_show: 15,
    current_path: [],
    commit_delta_margin: 6,

    render_all: function() {
        var last_path = this.current_path[ this.current_path.length - 1 ];

        if (last_path['kind'] === 'file')
            render_all_files( last_infos[last_path.val] );
    },

    create_all: function() {
        var last_path = this.current_path[ this.current_path.length - 1 ];

        if (last_path['kind'] === 'file')
            create_html_elements_for_all_files( last_infos[last_path.val] );
    },

    push_state: function(kind, value) {
        clear_display();
        this.current_path.push( {kind:kind, val: value});
    },

    pop_state: function( n ) {

        while (this.current_path.length - 1 != n)
            this.current_path.pop();

        clear_display();
        this.create_all();
        this.render_all();
    },

    get_previous: function() {
        var last_path = this.current_path[ this.current_path.length - 1 ];

        switch ( last_path['kind'] )
        {
        case 'file':
            fetch_previous_file(last_infos[last_path.val], last_path.val );
            break;

        case 'commit':
            fetch_full_commit(last_path.val[0]);
            break;
        }
    },

    push_last_commit: function(v) {
        this.current_path[ this.current_path.length - 1 ].val.unshift(v);
    }

};

var btn_toggle_text = {
    full: '&#x25bc;<br/>&#x25b2;',
    compact:  '&#x25b2;<br/>&#x25bc;'
};

function show_error( data )
{
    $('.message_carret').html( data.error )
}

////////////////////////////////////////////////////////////
////  Diff handling
////////////////////////////////////////////////////////////
var DiffManipulator = (function () {
    /** Generate an HTML representation of a diff and a file content.
     * @param filename filename of the file, used for syntax detection
     * @param contextSize Int
     * @param isLineNumberRequired {Bool}
     * @param data {String} file content.
     * @param diff [{way:String, beg:Int, end:Int}] diff ranges
     */
    var generate_compact_html = function (filename, contextSize, isLineNumberRequired, data, diff) {
        var highlighter = TinySyntaxHighlighter.from_filename(isLineNumberRequired, filename);
        var lines = data.split("\n");
        var begs = {
            '+': '<div class="diff_addition">',
            '-': '<div class="diff_deletion">',
            '~': '<div class="diff_addition"><div class="diff_deletion">',
            '!': '<div class="diff_deletion"><div class="diff_addition">',
            '|': ''
        };

        var ends = {
            '+': '</div>',       '-': '</div>',
            '~': '</div></div>', '!': '</div></div>',
            '|': ''
        };

        var processed_lines = [];
        var last_outputted_line = -1;
        var colorized;

        var i;
        for ( i = 0; i < diff.length; i++ )
        {
            var d = diff[i];
            
            // output the context before the diff
            var context_begin = Math.max(last_outputted_line + 1, d.beg - contextSize);

            // write an elipssiss if there is a deconnection
            if (context_begin > last_outputted_line + 1 && i != 0)
                processed_lines.push("...");

            highlighter.set_current_line_number(context_begin);
            for ( var lineNum = context_begin; lineNum < d.beg; lineNum++ )
            {
                processed_lines.push(highlighter.colorLine(lines[lineNum]));
            }

            // output the real diff
            if (d.end - d.beg == 0)
            {
                highlighter.set_current_line_number(d.beg);
                colorized = highlighter.colorLine(lines[d.beg]);
                processed_lines.push(begs[d.way] + colorized  + ends[d.way]);
            }
            else
            {
                highlighter.set_current_line_number(d.beg);
                colorized = highlighter.colorLine(lines[d.beg]);
                processed_lines.push( begs[d.way] + colorized);

                for ( var lineNum = d.beg + 1; lineNum < d.end; lineNum++ )
                    processed_lines.push(highlighter.colorLine(lines[lineNum]));

                processed_lines.push(highlighter.colorLine(lines[d.end]) + ends[d.way]);
            }

            var next_commit_begin = (i === diff.length - 1) ? lines.length - 1 : diff[i + 1].beg;
            var context_end = Math.min(d.end + contextSize, next_commit_begin - 1);
            highlighter.set_current_line_number(d.end + 1);
            for ( var lineNum = d.end + 1; lineNum <= context_end; lineNum++ ) {
                processed_lines.push(highlighter.colorLine(lines[lineNum]));
            }

            last_outputted_line = context_end;
        }

        return processed_lines.join("\n");
    }

    /** Combine two diff list into one list of edition
     * ranges.
     * @param removings [{way: String, orig_idx: Int, size: Int}]
     * @param addings   [{way: String, dest_idx: Int, size: Int}]
     * @return [{way: String, beg: Int, end: Int}]
     */
    var calculate_fold_set = function (removings, addings) {
        var ranges = [];
        var lefts = _.toArray(_.map(removings, function (c) { 
            return { way: '-', beg: c.orig_idx, end: c.orig_idx + c.size}
        }));

        var rights = _.toArray( _.map(addings, function (c) { 
                return { way: '+', beg: c.dest_idx, end: c.dest_idx + c.size}
        }));

        var combiner = function(a,b) {
            if (a == '+' && b == '-')
                { return '~' }
            else
                { return '!' }
        };

        var left_read_index = 0;
        var right_read_index = 0;

        var left = lefts[left_read_index];
        var right = rights[right_read_index];

        var swapArrays = function() {
            var swap_idx = left_read_index;
            left_read_index = right_read_index;
            right_read_index = swap_idx;

            var swap_array = lefts;
            lefts = rights;
            rights = swap_array;

            var swap_obj = left;
            left = right;
            right = swap_obj;
        }

        var inc_right = function() {
            right_read_index++;
            if (right_read_index < rights.length)
                right = rights[right_read_index];
        }

        var inc_left = function() {
            left_read_index++;
            if (left_read_index < lefts.length)
                left = lefts[left_read_index];
        }

        while (left_read_index < lefts.length && right_read_index < rights.length)
        {
            if (right.beg >= right.end) { inc_right(); }
            else if (left.beg >= left.end) { inc_left(); }
            else if (right.beg < left.beg) { swapArrays(); }
            // ############
            //                  ############
            else if (left.beg < right.beg && left.end < right.beg)
            {
                ranges.push({ way: left.way, beg: left.beg, end: left.end });
                inc_left();
            }
            else if (left.beg < right.beg && left.end <= right.end)
            {
                ranges.push({ way: left.way, beg: left.beg, end: right.beg - 1 });
                ranges.push({ way: combiner(left.way, right.way)
                        , beg: right.beg, end: left.end });
                right.beg = left.end + 1;
                inc_left();
            }
            else if (left.beg == right.beg)
            {
                if (left.end <= right.end )
                {
                    ranges.push({ way: combiner(left.way, right.way)
                            , beg: left.beg, end: left.end });
                    right.end = left.end + 1;
                    inc_left();
                }
                else
                {
                    ranges.push({ way: combiner(left.way, right.way)
                            , beg: left.beg, end: right.end });
                    left.beg = right.end + 1;
                    inc_right();
                }
            }
            else if (left.beg < right.beg && left.end > right.end)
            {
                ranges.push( {way: left.way, beg: left.beg, end: right.beg - 1  } );
                ranges.push({ way: combiner(left.way, right.way)
                            , beg: right.beg, end: right.end });
                left.beg = right.end + 1;
                inc_right();
            }
        }

        while (left_read_index < lefts.length)
            { ranges.push(lefts[left_read_index++]); }

        while (right_read_index < rights.length)
            { ranges.push(rights[right_read_index++]);  }

        return ranges;
    }

    var intercalate_diff_add = function (lines, diff_list) {
        for ( var idx in diff_list )
        {
            var diff = diff_list[idx];
            if (diff.way === "+")
            {
                lines[diff.dest_idx] = '<div class="diff_addition">' + lines[diff.dest_idx];
                lines[diff.dest_idx + diff.size] += '</div>';
            }
            
        }
        return lines;
    }

    var intercalate_diff_del = function (lines, diff_list) {
        for ( var idx in diff_list )
        {
            var diff = diff_list[idx];
            if (diff.way === "-")
            {
                lines[diff.orig_idx] = '<div class="diff_deletion">' + lines[diff.orig_idx];
                lines[diff.orig_idx + diff.size] += '</div>';
            }
            
        }
        return lines;
    }

    /** Keep only the diff information of adding (with a '+' way property)
     * @param diffs [{way: String, ...}]
     */
    var filter_adds = function(diffs) {
        return _.filter(diffs, function (c) { return c.way === '+'; })
    }

    /** Keep only the diff information of adding (with a '-' way property)
     * @param diffs [{way: String, ...}]
     */
    var filter_rems = function(diffs) {
        return _.filter(diffs, function (c) { return c.way === '-'; })
    }

    return {
        generateCompactHtml: generate_compact_html,
        intercalateDiffDel:  intercalate_diff_del,
        intercalateDiffAdd:  intercalate_diff_add,
        filterAdds: filter_adds,
        filterRems: filter_rems,
        calculateFoldSet: calculate_fold_set 
    };
})();

function render_commit( commit_collection, commit_number )
{
    var current_commit = commit_collection[commit_number];
    var prevs = commit_number <= 0 ? [] : commit_collection[commit_number - 1].diff;
    var neo_content = render_file( current_commit.file
                                 , prevs
                                 , current_commit.diff
                                 , current_commit.data);

    $('#' + current_commit.key + ' .file_content pre').html(neo_content);
}

function clear_display() {
    $('.container > *').remove();
}

function add_syntax_coloration( filename, lines )
{
    var ret = [];
    var highlighter = TinySyntaxHighlighter.from_filename(true, filename);

    for ( var i = 0; i < lines.length; i++ )
        ret.push(highlighter.colorLine(lines[i]));

    return ret;
}

function increase_context_size()
{
    application_state.context_size = application_state.context_size + 1;
    $('.toolbar div textarea').text(application_state.context_size);
    if (application_state.view_mode === 'compact')
        { application_state.render_all(); }
}

function decrease_context_size()
{
    application_state.context_size = Math.max(0, application_state.context_size - 1);
    $('.toolbar div textarea').text(application_state.context_size);
    if (application_state.view_mode === 'compact')
        { application_state.render_all(); }
}

function render_file(filename, prev_diff, diff, data)
{
    var rems = DiffManipulator.filterRems(diff);
    var adds = DiffManipulator.filterAdds(prev_diff);
    var ranges = DiffManipulator.calculateFoldSet(rems, adds);

    var clean_cr_lf_data = data.replace(/\r/g, '');

    if (application_state.view_mode === 'compact')
        return DiffManipulator.generateCompactHtml(filename, application_state.context_size,
                                                   true, clean_cr_lf_data, ranges);
    else // render full
    {
        var lines = add_syntax_coloration(filename, clean_cr_lf_data.split('\n'));
        var encoded_with_diff = DiffManipulator.intercalateDiffDel(lines, rems);
        var diffed_content = DiffManipulator.intercalateDiffAdd(encoded_with_diff, adds);
        return diffed_content.join('\n');
    }
}

function render_all_files( commit_collection )
{
    for ( var i in commit_collection )
        { render_commit(commit_collection, i); }
}

function toggle_diff_full()
{
    var ranges = null;

    if (application_state.view_mode === 'full')
        application_state.view_mode = 'compact';
    else
        application_state.view_mode = 'full';

    $('.btn_toggleview').html(
        btn_toggle_text[application_state.view_mode]);

    application_state.render_all();
}

function retrieve_commit_detail(commit_id) {
    $.getJSON('ask_commit/' + commit_id, {}, function(data) {
        if (data === null) {
            show_error({error: 'Communication error with the server'});
            return;
        }

        if (data['error']) { 
            show_error( data );
            return;
        }

        var kind_formater = {
            'modification': ich.commit_file_modification,
            'addition':ich.commit_file_addition,
            'deletion':ich.commit_file_deletion
        };

        var detail = $('#' + commit_id + ' .commit_detail');
        detail.append(ich.commit_button_file({commit: commit_id}));

        for ( var change in data )
        {
            var e = data[change];
            var kind = e['kind'];

            var file_diff;
            if (kind_formater.hasOwnProperty(kind))
                detail.append(kind_formater[kind](e));
            else
                detail.append(ich.commit_file_unknown(e));
        }
    });
}

function append_breadcrumb( name ) {
    $('#breadcrumb').append(ich.breadcrumbelem({name:name, id:0}));
}

function switch_break(id) {
    application_state.pop_state(id);
}

function switch_to_commit(commit_id) {
    application_state.push_state('commit', [commit_id]);
    fetch_full_commit(commit_id);
    append_breadcrumb(commit_id);
}

function fetch_full_commit(commit_id) {
    $.getJSON('commit/' + commit_id, {}, function(data) {

        orig_node = ich.commit_detailed(data);

        var kind_formater = {
            'modification': function(e) {
                var hl = TinySyntaxHighlighter.from_filename(true, e.name);
                var rez_node = ich.commit_file_modification_detailed(e);
                var code_node = rez_node.find('pre');

                var curr_diff;
                var acc = ""
                for ( var i = 0; i < e.diff.length; i++ )
                {
                    curr_diff = e.diff[i];
                    if (curr_diff.way == '+') {
                        acc += '<div class="diff_addition">';
                        hl.set_current_line_number(curr_diff.dest_idx);
                    }
                    else {
                        acc +=  '<div class="diff_deletion">';
                        hl.set_current_line_number(curr_diff.orig_idx);
                    }

                    for ( var l = 0; l < curr_diff.data.length; l++ )
                        acc += hl.colorLine(curr_diff.data[l])

                    if (i < e.diff.length - 1)
                        acc += "</div>\n...\n";
                }

                code_node.html(acc);

                return rez_node;
            },
            'addition':ich.commit_file_addition,
            'deletion':ich.commit_file_deletion
        };

        for ( var change in data.file_changes )
        {
            var e = data.file_changes[change];
            var kind = e['kind'];

            var file_diff;
            if (kind_formater.hasOwnProperty(kind))
                orig_node.append(kind_formater[kind](e));
            else
                orig_node.append(ich.commit_file_unknown(e));
        }

        application_state.push_last_commit(data.parents_sha[0]);

        $('.container').prepend(orig_node);
    });
}

function create_html_elements_for_all_files(commits) {
    for ( var i in commits )
    {
        var e = commits[i];
        create_html_elements_for_file(e);
    }
}

function create_html_elements_for_file(last_commit) {
    last_commit.short_message = last_commit.message.split("\n")[0];
    var processed = ich.commitfile(last_commit);
    $(".container").prepend( processed );
}

function fetch_previous_file(commit_collection, path)
{
    var last_commit = commit_collection[0];
                 // commit: Hash
    var params = { commit: last_commit.parent_commit
                 // last_file: Hash
                 , last_file: last_commit.filekey 
                 };

    $.getJSON('ask_parent/' + last_commit.file, params, function(data) {
        if (data === null) {
            show_error({error: 'Communication error with the server'});
            return;
        }

        if (data['error']) { 
            show_error( data );
            return;
        }

        var new_commit = {
                       file: last_commit.file  
            ,          diff: data.diff
            ,          data: data.data
            ,       filekey: data.filekey
            ,           key: data.key
            ,       message: data.message
            , parent_commit: data.parent_commit
            ,          path: data.path
        };

        create_html_elements_for_file( new_commit );
        commit_collection.unshift( new_commit );

        render_commit( commit_collection, 0 );
        render_commit( commit_collection, 1 );
    });
}

function render_initial_document( filename ) {
    var infos = last_infos[filename];
    create_html_elements_for_file( infos[0] );
    render_commit( infos, 0 );
    $("#breadcrumb").append(ich.breadcrumbelem({id:0, name:filename}));
}

function leave_server()
{
    $.ajax( {url:"quit", async:false} );
}

ich.grabTemplates();
