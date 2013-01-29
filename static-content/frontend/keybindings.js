$(document).bind('keydown', 'h', function(){
    Project.state.send_state_message({action: Project.GuiMessage.MOVE_LEFT});
});

$(document).bind('keydown', 'j', function(){
    Project.state.send_state_message({action: Project.GuiMessage.MOVE_DOWN});
});

$(document).bind('keyup', 'down', function(){
    Project.state.send_state_message({action: Project.GuiMessage.MOVE_DOWN});
});

$(document).bind('keydown', 'k', function(){
    Project.state.send_state_message({action: Project.GuiMessage.MOVE_UP});
});

$(document).bind('keyup', 'up', function(){
    Project.state.send_state_message({action: Project.GuiMessage.MOVE_UP});
});

$(document).bind('keydown', 'l', function(){
    Project.state.send_state_message({action: Project.GuiMessage.MOVE_RIGHT});
});

$(document).bind('keydown', '0', function(){
    Project.state.send_state_message({action: Project.GuiMessage.MOVE_FIRST});
});

$(document).bind('keydown', 'Shift+0', function(){
    Project.state.send_state_message({action: Project.GuiMessage.MOVE_FIRST});
});

$(document).bind('keydown', 'Shift+4', function(){
    Project.state.send_state_message({action: Project.GuiMessage.MOVE_LAST});
});

$(document).bind('keydown', 'return', function(){
    Project.state.send_state_message({action: Project.GuiMessage.MOVE_INNER});
});

$(document).bind('keydown', ':', function() {
    Project.state.send_state_message({action: Project.GuiMessage.COMMAND_REQUEST});
});

$(document).bind('keydown', 's', function() {
    Project.state.send_state_message({action: Project.GuiMessage.COMMAND_REQUEST});
});

$(document).bind('keyup', 'esc', function() {
    Project.state.send_state_message({action: Project.GuiMessage.ESCAPE});
});

$(document).bind('keydown', 'ctrl+left', function() {
    breadcrumb.go_backward();
});

$(document).bind('keyup', '1', function() {
    Project.state.send_state_message({action: Project.GuiMessage.SELECT_AS_LEFT});
});

$(document).bind('keyup', '2', function() {
    Project.state.send_state_message({action: Project.GuiMessage.SELECT_AS_RIGHT});
});

$(document).bind('keyup', 'b', function() {
    Project.state.send_state_message({action: Project.GuiMessage.SWITCH_BLAME});
});

$(document).bind('keydown', 'ctrl+right', function() {
    breadcrumb.go_forward();
});

