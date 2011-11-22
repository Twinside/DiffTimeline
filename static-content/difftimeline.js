/////////////////////////////////////////////////////////////////////
//              Initial state
/////////////////////////////////////////////////////////////////////
var application_state = {  
    view_mode: 'full',
    context_size: 2
};

var btn_toggle_text = {
    full: '&#x25bc;<br/>&#x25b2;',
    compact:  '&#x25b2;<br/>&#x25bc;'
}

/////////////////////////////////////////////////////////////////////
//              HTML generation
/////////////////////////////////////////////////////////////////////
function div_class(classname) {
    ret = document.createElement('div');
    ret.setAttribute('class', classname);
    return ret;
}

function img(src) {
    ret = document.createElement('img');
    ret.setAttribute('src', src);
    return ret;
}

function pre(content) {
    ret = document.createElement('pre');
    ret.innerHTML = content;
    return ret;
}

function show_error( data ) { $('.message_display').html( data.error ) }

function build_commit_delta(lst) {
    var node = div_class('commit_list');
    for ( var i = lst.length - 1; i >= 0; i-- )
    {
        var interval_commit = lst[i];
        var new_commit = div_class('delta_commit_circle');

        var msg_tooltip = document.createElement('div');
        msg_tooltip.innerHTML = interval_commit.message;
        new_commit.appendChild(msg_tooltip);
        node.appendChild(new_commit);
    }

    return node;
}

function build_commit_message_node(commit, message) {
    var msg = div_class('commitmsg');
    var short_message = message.split("\n")[0];
    msg.innerHTML = '<span class="id">' + commit + '</span><hr /><h4 title="' + message + '">' + short_message + '</h4>';

    return msg;
}

function div_sub( cl, lst ) {
    var node = div_class(cl);
    for ( var i in lst ) { node.appendChild( lst[i] ); }
    return node;
}

////////////////////////////////////////////////////////////
////  Diff handling
////////////////////////////////////////////////////////////
var DiffManipulator = (function () {
    /** Generate an HTML representation of a diff and a file content.
     * @param contextSize Int
     * @param isLineNumberRequired {Bool}
     * @param data {String} file content.
     * @param diff [{way:String, beg:Int, end:Int}] diff ranges
     */
    var generate_compact_html = function (contextSize, isLineNumberRequired, data, diff) {
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
        var line_number_string = function (i) {
            if (!isLineNumberRequired) return '';
            return '<span class="line_number">' + (i + 1).toString() + '</span>'
        }

        var last_outputted_line = -1;

        var i;
        for ( i = 0; i < diff.length; i++ )
        {
            var d = diff[i];
            
            // output the context before the diff
            var context_begin = Math.max(last_outputted_line + 1, d.beg - contextSize);

            // write an elipssiss if there is a deconnection
            if (context_begin > last_outputted_line + 1 && i != 0)
                processed_lines.push("...");

            for ( var lineNum = context_begin; lineNum < d.beg; lineNum++ )
                processed_lines.push(line_number_string(lineNum) + lines[lineNum]);

            // output the real diff
            if (d.end - d.beg == 0)
            {
                processed_lines.push(begs[d.way] + line_number_string(d.beg) +
                                     lines[d.beg] + ends[d.way]);
            }
            else
            {
                processed_lines.push( begs[d.way] + line_number_string(d.beg) +
                                    lines[d.beg]);

                for ( var lineNum = d.beg + 1; lineNum < d.end; lineNum++ )
                    processed_lines.push(line_number_string(d.beg) + lines[lineNum]);

                processed_lines.push(line_number_string(d.end) + lines[d.end] + ends[d.way]);
            }

            var next_commit_begin = (i === diff.length - 1) ? lines.length - 1 : diff[i + 1].beg;
            var context_end = Math.min(d.end + contextSize, next_commit_begin - 1);
            for ( var lineNum = d.end + 1; lineNum <= context_end; lineNum++ )
                processed_lines.push(line_number_string(lineNum) + lines[lineNum]);

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

function render_commit( commit_number )
{
    var current_commit = last_infos[commit_number];
    var prevs = commit_number <= 0 ? [] : last_infos[commit_number - 1].diff;
    var neo_content = render_file(prevs, current_commit.diff, current_commit.data);
    $('#' + current_commit.key + ' .file_content pre').html(neo_content);
}

function add_line_number( lines )
{
    var ret = [];
    for ( var i = 0; i < lines.length; i++ )
        ret.push('<span class="line_number">' + (i + 1).toString() + '</span>' + lines[i]);

    return ret;
}

function render_file(prev_diff, diff, data)
{
    var rems = DiffManipulator.filterRems(diff);
    var adds = DiffManipulator.filterAdds(prev_diff);
    var ranges = DiffManipulator.calculateFoldSet(rems, adds);

    if (application_state.view_mode === 'compact')
        return DiffManipulator.generateCompactHtml(application_state.context_size, true, data, ranges);
    else // render full
    {
        var lines = add_line_number(data.split('\n'));
        var encoded_with_diff = DiffManipulator.intercalateDiffDel(lines, rems);
        var diffed_content = DiffManipulator.intercalateDiffAdd(encoded_with_diff, adds);
        return diffed_content.join('\n');
    }
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


    for ( var i in last_infos )
        { render_commit(i); }
}

function fetch_image(e)
{
    var miniatures = document.getElementById('miniatures');
    var src = ('miniature/' + e.file + '?commit=' + e.parent_commit + '&last_file=' +
                e.filekey);
    miniatures.insertBefore(img(src), miniatures.childNodes[0]);
}

function back_to_the_past() 
{
    var last_commit = last_infos[0];
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

        var container = document.getElementById('container');

        var commit = div_class('commit');
        commit.setAttribute('id', last_commit.parent_commit);

        var commitmsg = build_commit_message_node(last_commit.parent_commit, data.message);
        var deltas = build_commit_delta(data.path);
        var commit_info = div_sub("commitinfo", [commitmsg, deltas]);

        var encoded = data.data.replace(/\&/g, '\&amp;').replace(/</g, '\&lt;').replace(/</g, '\&gt;');
        var content = div_class('file_content');
        content.appendChild(pre(''));

        commit.appendChild(commit_info);
        commit.appendChild(content);

        var add_content = $('#' + last_commit.key + ' .file_content').get()[0];

        container.insertBefore(commit, container.childNodes[0]);


        var new_commit = {
                       file: last_commit.file  
            ,          diff: data.diff
            ,          data: encoded
            ,       filekey: data.filekey
            ,           key: last_commit.parent_commit
            , parent_commit: data.parent_commit
        };
        last_infos.unshift( new_commit );

        render_commit( 0 );
        render_commit( 1 );

        fetch_image(new_commit);
    });
}

function leave_server()
{
    $.ajax( {url:"quit", async:false} );
}

