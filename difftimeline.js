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

function build_commit_delta(node, lst)
{
    for ( var i = lst.length - 1; i >= 0; i-- )
    {
        var interval_commit = lst[i];
        var new_commit = div_class('delta_commit_circle');

        var msg_tooltip = document.createElement('div');
        msg_tooltip.innerHTML = interval_commit.message;
        new_commit.appendChild(msg_tooltip);
        node.appendChild(new_commit);
    }
}

function back_to_the_past() 
{
    var last_commit = last_infos[ last_infos.length - 1 ];
    var params = { commit: last_commit.parent_commit
                 , last_file: last_commit.filekey };

    $.getJSON('ask_parent', params, function(data) {
        if (data['error'])
        { 
            show_error( data );
            return;
        }

        var container = document.getElementById('container');

        var commit = div_class('commit');
        commit.setAttribute('id', last_commit.parent_commit);

        var commit_delta = div_class('commit_list');
        build_commit_delta(commit_delta, data.path);

        var msg = div_class('commitmsg');
        msg.innerHTML = data.message;

        var rawData = data.data;
        var encoded = rawData.replace(/\&/g, '\&amp;').replace(/</g, '\&lt;').replace(/</g, '\&gt;');
        var encoded_with_diff = intercalate_diff_del(encoded, data.diff);
        var content = div_class('file_content');
        content.appendChild(pre(encoded_with_diff));

        commit.appendChild(msg);
        commit.appendChild(commit_delta);
        commit.appendChild(content);

        var add_content = $('#' + last_commit.key + ' .file_content').get()[0];
        add_content.innerHTML = intercalate_diff_add(add_content.innerHTML, data.diff);

        container.insertBefore(commit, container.childNodes[0]);

        last_infos.push( {
                       file: last_commit.file  
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

