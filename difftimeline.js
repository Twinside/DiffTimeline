
function back_to_the_past() {
    $.getJSON('ask_parent', last_infos.parent_commit, function(data) {
        alert(data);
    })
}

function leave_server() {
    $.ajax({url:"quit", async:false})
}

