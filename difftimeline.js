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

function back_to_the_past() {
    $.getJSON('ask_parent', last_infos.parent_commit, function(data) {
        last_infos.parent_commit = data.parent_commit;

        var container = document.getElementById('container');

        var commit = div_class('commit');

        var msg = div_class('commitmsg');
        msg.innerHTML = data.message;

        var rawData = data.data;
        encoded = rawData.replace(/\&/g, '\&amp;').replace(/</g, '\&lt;').replace(/</g, '\&gt;');
        var content = div_class('file_content');
        content.appendChild(pre(encoded));

        commit.appendChild(msg);
        commit.appendChild(content);

        container.appendChild( commit );
    })
}

function leave_server() {
    $.ajax({url:"quit", async:false})
}

