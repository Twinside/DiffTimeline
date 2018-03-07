/// <reference path="jquery.d.ts" />
/// <reference path="difftimeline.extern.ts" />

type ref = string;

namespace ich {
    export var branch_list : (data: {ref_list: any}) => JQuery;
    export var breadcrumbelem : (data: {name: string, id: number}) => JQuery;
    export var blamefile: (data : BlameInfo) => JQuery;
    export var commit_file: (data: any) => JQuery;
    export var commitfile: (data: any) => JQuery;
    export var commit_file_modification_detailed: (data: CommitTreeDiff) => JQuery;
    export var tree_folder: (data: CommitTreeDiff) => JQuery;
    export var tree_elem: (data: any) => JQuery;
    export var commit_detailed: (data: any) => JQuery;
    export var commit_compact: (data: any) => JQuery;
    export var fetch_previous: (data: any) => JQuery;
    export var file_widget: (data: any) => JQuery;
    export var commit_file_modification: (data: any) => JQuery;
    export var commitfile_huge_path: (data: any) => JQuery;
    export var file_search_result: (data: any) => JQuery;
    export var compare_files : (data: any) => JQuery;
    export var key_help: (data: { keys: KeyBinding[] }) => JQuery;

    export var grabTemplates: () => void;
}

ich.grabTemplates();

const null_ref : ref = "0000000000000000000000000000000000000000";
const display_null_ref : string = 'Working directory';
const working_dir_request_token : string = '__WORKING_DIR__';
const sub_focus : string = 'focused_diff';
const global_focus : string = 'focused_commit';


function make_draggable_elems(node : Node | Element | JQuery) {
    $('.file_widget', node).draggable({ 
        helper: 'clone',
        appendTo: 'body',
        start: (event : any, ui : any) => { $(this).css("z-index", 15); },
        zIndex: 300
    });

    $('.branch_widget', node).draggable({
        appendTo: 'body',
        zIndex: 300,
        start: (event : any, ui : any) => { $(this).css("z-index", 15); },
        helper: 'clone'
    });
}

const blame_gradient = {
    beg: { r: 0x00, g:0x00, b:0x00 },
    end: { r: 0x80, g:0x80, b:0x80 }
};

type color = {r: number, g: number, b: number };

var color_lerp = function ( c1 : color, c2 : color, v : number ) {
    var lerp = function( v1 : number, v2 : number ) : number {
        return Math.floor(v2 * v + v1 * (1 - v));
    };

    return ('rgb(' + lerp(c1.r, c2.r) + ', '
                   + lerp(c1.g, c2.g) + ', '
                   + lerp(c1.b, c2.b) + ')');
};

function remove_children(node : Node) {
    while (node.hasChildNodes()) {
        node.removeChild(node.lastChild);
    }
};

function timestamp_to_string(stamp : number) : string {
    var d = new Date();
    d.setTime(stamp * 1000);
    return d.toLocaleDateString() + " " + d.toLocaleTimeString();
}

/**
 * @param {string} snipp
 * @return {string}
 */
function html_encodize(snipp : string) : string {
    return snipp.replace(/\&/g, '\&amp;').replace(/</g, '\&lt;').replace(/</g, '\&gt;');
}

function show_error( data : { error: string } ) {
    $('.message_carret').html( data.error )
}

function leave_server()
    { $.ajax( {url:"/quit", async:false} ); }

function nop() {}
