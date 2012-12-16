/** @typedef (string) */
var ref;

/**
 * @const
 * @type {Ref}
 */
var null_ref = "0000000000000000000000000000000000000000";

/**
 * @const
 * @type {string}
 */
var display_null_ref = 'Working directory';

/**
 * @const
 * @type {string}
 */
var working_dir_request_token = '__WORKING_DIR__';

/**
 * @const
 * @type {string}
 */
var sub_focus = 'focused_diff';

/**
 * @const
 * @type {string}
 */
var global_focus = 'focused_commit';

/**
 * @type {function(jQuery) : void}
 */
function make_draggable_elems(node) {
    $('.file_widget', node).draggable({ 
        helper: 'clone',
        appendTo: 'body',
        start: function(event, ui) { $(this).css("z-index", 15); },
        zIndex: 300
    });

    $('.branch_widget', node).draggable({
        appendTo: 'body',
        zIndex: 300,
        start: function(event, ui) { $(this).css("z-index", 15); },
        helper: 'clone'
    });
}

var blame_gradient = {
    beg: { r: 0x00, g:0x00, b:0x00 },
    end: { r: 0x80, g:0x80, b:0x80 }
};

var color_lerp = function ( c1, c2, v ) {
    var lerp = function( v1, v2 ) {
        return Math.floor(v2 * v + v1 * (1 - v));
    };

    return ('rgb(' + lerp(c1.r, c2.r) + ', '
                   + lerp(c1.g, c2.g) + ', '
                   + lerp(c1.b, c2.b) + ')');
};

/**
 * @param {Element} node
 * @return {void}
 */
var remove_children = function(node) {
    while (node.hasChildNodes()) {
        node.removeChild(node.lastChild);
    }
};

/**
 * @param {number} stamp
 * @return {string}
 */
var timestamp_to_string = function(stamp) {
    var d = new Date();
    d.setTime(stamp * 1000);
    return d.toLocaleDateString() + " " + d.toLocaleTimeString();
}

/**
 * @param {string} snipp
 * @return {string}
 */
var html_encodize = function(snipp) {
    return snipp.replace(/\&/g, '\&amp;').replace(/</g, '\&lt;').replace(/</g, '\&gt;');
}

function show_error( data ) {
    $('.message_carret').html( data.error )
}

function leave_server()
    { $.ajax( {url:"/quit", async:false} ); }

