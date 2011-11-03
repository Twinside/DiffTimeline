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

function toggle_diff_full(i)
{
    var ranges = [];
    var rems = _.filter(last_infos[i].data, function (c) {
        return c.way == '-';
    }).map(function (c) { return $.extend(true, {}, c) }).toArray();

    var adds = _.filter(last_infos[i - 1].data, function (c) {
        return c.way == '+';
    }).map(function (c) { return $.extend(true, {}, c) }).toArray();

    var rems_read_index = 0;
    var adds_read_index = 0;

    var rem = rems[rems_read_index];
    var add = add[adds_read_index];

    while (rems_read_index < rems.length && adds_read_index < adds.length)
    {
        add = add[adds_read_index];

        // rem before add
        if (rem.orig_idx < add.dest_idx)
        {
            // full before
            if (rem.orig_idx + rem.size < add.dest_idx)
            {
                ranges.add({way: '-', beg: rem.orig_dix, end: rem.orig_idx + rem.size });
                rem = rems[++rems_read_index];
            }
            // end before the end of other
            else if ()
            {
            }
        }
        // add <= rem
        else
        {
            if (rem.orig_idx > add.dest_idx + add.size)
            {
                ranges.add({way: '+', beg: add.orig_dix, end: add.orig_idx + add.size });
            }
            
        }

    }

    if (rems_read_index < rems.length )
    {
        while (rems_read_index < rems.length)
        {
            var idx = rems[rems_read_index].orig_idx;
            var s = rems[rems_read_index].size;
            ranges.add({ way: '-', beg: idx, end:  idx + s });
            rems_read_index++;
        }
    }

    if (adds_read_index < adds.length)
    {
        while (adds_read_index < adds.length)
        {
            var idx = adds[adds_read_index].dest_idx;
            var s = adds[adds_read_index].size;
            ranges.add({ way: '+', beg: idx, end:  idx + s });
            adds_read_index++;
        }
    }
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

