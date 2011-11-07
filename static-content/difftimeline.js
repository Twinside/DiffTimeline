/////////////////////////////////////////////////////////////////////
//              HTML generation
/////////////////////////////////////////////////////////////////////
function div_class(classname)
{
    ret = document.createElement('div');
    ret.setAttribute('class', classname);
    return ret;
}

function pre(content)
{
    ret = document.createElement('pre');
    ret.innerHTML = content;
    return ret;
}

function intercalate_diff_add(content, diff_list)
{
    var lines = content.split("\n");

    for ( var idx in diff_list )
    {
        var diff = diff_list[idx];
        if (diff.way === "+")
        {
            lines[diff.dest_idx] = '<div class="diff_addition">' + lines[diff.dest_idx];
            lines[diff.dest_idx + diff.size] += '</div>';
        }
        
    }
    return lines.join("\n");
}

function intercalate_diff_del(content, diff_list)
{
    var lines = content.split("\n");

    for ( var idx in diff_list )
    {
        var diff = diff_list[idx];
        if (diff.way === "-")
        {
            lines[diff.orig_idx] = '<div class="diff_deletion">' + lines[diff.orig_idx];
            lines[diff.orig_idx + diff.size] += '</div>';
        }
        
    }
    return lines.join("\n");
}

function show_error( data )
{
    var container = document.getElementById('message_display');
    container.innerHTML = data.error;
}

function build_commit_delta(lst)
{
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

function build_commit_message_node(commit, message)
{
    var msg = div_class('commitmsg');
    var short_message = message.split("\n")[0];
    msg.innerHTML = '<span class="id">' + commit + '</span><hr /><h4 title="' + message + '">' + short_message + '</h4>';

    return msg;
}

function div_sub( cl, lst )
{
    var node = div_class(cl);
    for ( var i in lst ) { node.appendChild( lst[i] ); }
    return node;
}

////////////////////////////////////////////////////////////
////  Diff handling
////////////////////////////////////////////////////////////
function generate_compact_html(data, diff)
{
    var lines = data.split("\n");
    var begs = {
        '+': '<div class="diff_addition">',
        '-': '<div class="diff_deletion">',
        '~': '<div class="diff_addition"><div class="diff_deletion">',
        '!': '<div class="diff_deletion"><div class="diff_addition">'
    };

    var ends = {
        '+': '</div>',       '-': '</div>',
        '~': '</div></div>', '!': '</div></div>'
    };

    var processed_lines = [];

    for ( var i in diff )
    {
        var d = diff[i];
        
        if (d.end - d.beg == 0)
        {
            processed_lines.push(begs[d.way] + lines[d.beg - 1] + ends[d.way]);
            continue;
        }

        processed_lines.push(begs[d.way]);
        for ( var lineNum = d.beg + 1; lineNum <= d.end - 1; lineNum++ )
            processed_lines.push(lines[lineNum]);
        processed_lines.push(ends[d.way] + "\n...\n");
    }

    return processed_lines.join("\n");
}

function toggle_diff_full()
{
    var ranges = null;

    for ( var i in last_infos )
    {
        var key = last_infos[i].key;
        var cont = $('#' + key + ' .file_content pre');
        ranges = calculate_fold_set(i);
        var new_content = generate_compact_html(last_infos[i].data, ranges);
        cont.html(new_content);
    }
}

/** Combine two diff list into one list of edition
 * ranges.
 */
function calculate_fold_set(i)
{
    var ranges = [];
    var lefts = _.toArray(_.filter(last_infos[i].diff, function (c) {
        return c.way == '-';
    }).map(function (c) { return { way: '-'
                                 , beg: c.orig_idx
                                 , end: c.orig_idx + c.size}}));

    var rights = [];
    if (i > 0)
    {
        rights = _.toArray( _.filter(last_infos[i - 1].diff, function (c) {
            return c.way == '+';
        }).map(function (c) { return { way: '+'
                                     , beg: c.dest_idx
                                     , end: c.dest_idx + c.size}}));
    }

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

function back_to_the_past() 
{
    var last_commit = last_infos[ last_infos.length - 1 ];
    var params = { commit: last_commit.parent_commit
                 , last_file: last_commit.filekey };

    $.getJSON('ask_parent', params, function(data) {
        if (data === null)
        {
            show_error({error: 'Communication error with the server'});
            return;
        }

        if (data['error'])
        { 
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
        var encoded_with_diff = intercalate_diff_del(encoded, data.diff);
        var content = div_class('file_content');
        content.appendChild(pre(encoded_with_diff));

        commit.appendChild(commit_info);
        commit.appendChild(content);

        var add_content = $('#' + last_commit.key + ' .file_content').get()[0];
        add_content.innerHTML = intercalate_diff_add(add_content.innerHTML, data.diff);

        container.insertBefore(commit, container.childNodes[0]);

        last_infos.push( {
                       file: last_commit.file  
            ,          diff: data.diff
            ,          data: encoded
            ,       filekey: data.filekey
            ,           key: last_commit.parent_commit
            , parent_commit: data.parent_commit
        });
    });
}

function leave_server()
{
    $.ajax( {url:"quit", async:false} );
}

