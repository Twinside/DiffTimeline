
var Project = {};

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
        zIndex: 300,
    });

    $('.branch_widget', node).draggable({
        appendTo: 'body',
        zIndex: 300,
        start: function(event, ui) { $(this).css("z-index", 15); },
        helper: 'clone'
    });
}

/////////////////////////////////////////////////////////////////////
//              Initial state
/////////////////////////////////////////////////////////////////////
/**
 * @namespace
 */
var breadcrumb = (function() {
    "use strict";

    /** @type {number} */
    var count = 0;

    /** @type {number} */
    var current_index = 0;

    return {
        append_breadcrumb:
		/** @type {function(string) : void} */
						   function( name ) {
            if (current_index < count - 1)
            {
                $('#breadcrumb > span').slice(current_index + 1).remove();
                count = current_index + 1;
            }

            /** @type {jQuery} */
            var elem = ich.breadcrumbelem({name:name, id:count});
            $('#breadcrumb').append(elem);
            current_index = count++;
        },

        go_forward: function() {
            if (current_index === count - 1)
                return;

            current_index++;
            Project.state.jump_context(current_index);
        },

        go_backward: function() {
            if (current_index === 0)
                return;

            current_index--;
            Project.state.jump_context(current_index);
        },

        click_index: 
        /** @type {function(number) : void} */
					function( idx ) {
            current_index = idx;
            Project.state.jump_context(idx);
        }
    };
})();

/** @interface */
var ResultSet = function() {};

ResultSet.prototype.create_all_dom = function() {};
ResultSet.prototype.render_all = function() {};

ResultSet.prototype.send_message = function( msg ) {};
ResultSet.prototype.fetch_previous = function() {};

/** @enum {number} */
Project.ViewMode = {
    VIEW_FULL: 1,
    VIEW_COMPACT: 0
};

/**
 * @const
 * @enum {string}
 */
Project.DiffChar = {
    DIFF_ADD: '+',
    DIFF_DEL: '-',
    DIFF_DELADD: '~',
    DIFF_ADDDEL: '!'
};

/**
 * @const
 * @enum {string}
 */
Project.DiffKind = {
    KIND_MODIFICATION: 'modification',
    KIND_ADDITION: 'addition',
    KIND_DELETION: 'deletion'
}

/**
 * @const
 * @enum {number}
 */
Project.GuiMessage = {
    FETCH_TREE:   0,
    FETCH_DETAIL: 1,
    MOVE_LEFT:    2,
    MOVE_RIGHT:   3,
    MOVE_UP:      4,
    MOVE_DOWN:    5,
    MOVE_FIRST:   6,
    MOVE_LAST:    7,

    MOVE_INNER:   8
}

var blame_gradient = {
    beg: { r: 0x0, g:0x0, b:0x0},
    end: { r: 0x80, g:0x80, b:0x80},
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
 * @type {Object}
 */
Project.state = (function () {
    "use strict";

    /** @type {Project.ViewMode} */
    var view_mode = Project.ViewMode.VIEW_FULL;

    /** @type {boolean} */
    var apply_syntax_coloration = true;

    /** @type {number} */
    var context_size = 2;

    /** @type {number} */
    var max_commit_delta_show = 15;

    /** @type {Array.<ResultSet>} */
    var states = [];

    /** @type {Array.<ResultSet>} */
    var forward_states = [];

    /** @type {number} */
    var commit_delta_margin = 6;

    /** @type {number} */
    var apparition_duration = 750;

    /** @type {number} */
    var apparition_pane_duration = 500;

    /** @type {Object.<Project.ViewMode, string>} */
    var btn_toggle_text = {}
    btn_toggle_text[Project.ViewMode.VIEW_FULL] =  '&#x25bc;<br/>&#x25b2;';
    btn_toggle_text[Project.ViewMode.VIEW_COMPACT] =  '&#x25b2;<br/>&#x25bc;';

    var show_hide_toolbar_elements = function(descr) {
        if (descr.compact_view)
            $('.btn_toggleview').show();
        else
            $('.btn_toggleview').hide();

        if (descr.fetch_previous)
            $('.btn_returnpast').show();
        else
            $('.btn_returnpast').hide();

        if (descr.context_size)
            $('.context_size').show();
        else
            $('.context_size').hide();

        if (descr.syntax_toggle)
            $('.syntax_highlight_toggle').show();
        else
            $('.syntax_highlight_toggle').hide();

    };

    return {
        /** @type {function() : Project.ViewMode} */
        active_view_mode: function() { return view_mode; },

        /** @type {function() : number} */
        active_context_size: function() { return context_size; },

        /** @type {function() : number} */
        apparition_duration: function() { return apparition_duration; },

        /** @type {function(string) : void} */
        send_state_message: function(message) {
            var last_path = states[ states.length - 1 ];
            last_path.send_message(message);
        },

        /** @type {function() : void} */
        create_all_dom: function() {
            var last_path = states[ states.length - 1 ];
            show_hide_toolbar_elements(last_path.gui_descr);
            last_path.create_all_dom();
        },

        render_all: function() {
            var last_path = states[ states.length - 1 ];
            last_path.render_all();
        },

        get_previous: function() {
            var last_path = states[ states.length - 1 ];
            last_path.fetch_previous();
        },

        /**
         * @type {function(ref, string) : void}
         */
        switch_blame: function(start_commit, file) {
            this.clear_display();
            var new_state = BlameShower.create_from_arg(start_commit, file);
            states.push( new_state );
            show_hide_toolbar_elements(new_state.gui_descr);
            breadcrumb.append_breadcrumb("Blame (" + file + ")");
        },

        /**
         * @type {function(string, ref, ref) : void}
         */
        switch_file: function(file, fkey, start_commit) {
            this.clear_display();
            var new_state = FileRenderer.create_from_arg(file, fkey, start_commit);
            states.push( new_state );
            show_hide_toolbar_elements(new_state.gui_descr);
            breadcrumb.append_breadcrumb(file);
        },

        switch_commit_comp: function( b1, b2 ) {
            this.clear_display();
            var new_state = CommitComparer.create_from_args(b1, b2);
            states.push( new_state );
            show_hide_toolbar_elements(new_state.gui_descr);
            breadcrumb.append_breadcrumb('Compare commit');
        },

        switch_file_comp: function(key1, file1, key2, file2) {
            this.clear_display();
            var new_state = FileComparer.create_from_args(key1, file1, key2, file2);
            states.push( new_state );
            show_hide_toolbar_elements(new_state.gui_descr);
            breadcrumb.append_breadcrumb('Compare file');
        },

        /** @type {function(ref) : void} */
        switch_commit: function(id) {
            this.clear_display();
            var new_state = new CommitRenderer.create_from_arg(id);
            states.push(new_state);

            if (id !== null_ref)
                breadcrumb.append_breadcrumb(id);
            else
                breadcrumb.append_breadcrumb('HEAD');

            show_hide_toolbar_elements(new_state.gui_descr);
        },

        /** @type {function(number) : void} */
        jump_context: function(idx) {
            if (states.length - 1 == idx) return;

            var i;

            if (idx < states.length - 1) {
                while (idx != states.length - 1)
                    forward_states.push(states.pop());
            } else {
                while (idx != states.length - 1)
                    states.push(forward_states.pop());
            }

            this.clear_display();
            this.create_all_dom();
            this.render_all();
        },

        clear_display: function () {
            $('.container > *').remove();
        },

        increase_context_size: function() {
            context_size = context_size + 1;
            $('.toolbar div textarea').text(context_size.toString());

            if (view_mode === Project.ViewMode.VIEW_COMPACT) { this.render_all(); }
        },

        decrease_context_size: function() {
            context_size = Math.max(0, context_size - 1);
            $('.toolbar div textarea').text(context_size.toString());

            if (view_mode === Project.ViewMode.VIEW_COMPACT) { this.render_all(); }
        },

        toggle_diff_full: function() {
            var ranges = null;

            if (view_mode === Project.ViewMode.VIEW_FULL)
                view_mode = Project.ViewMode.VIEW_COMPACT;
            else view_mode = Project.ViewMode.VIEW_FULL;

            $('.btn_toggleview').html(btn_toggle_text[view_mode]);
            this.render_all();
        },

        chrome_scroll_offset: function() {
            return {top:-120, left:-120};
        },

        start_blame: function(blame_obj) {
            var new_state = BlameShower.create_from_data(blame_obj);
            states.push( new_state );
            breadcrumb.append_breadcrumb('Blame (' + blame_obj.filename + ')');
            show_hide_toolbar_elements(new_state.gui_descr);
        },

        start_commit: function(commit_obj) {
            var new_state = CommitRenderer.create_from_data(commit_obj);
            states.push( new_state );
            show_hide_toolbar_elements(new_state.gui_descr);

            if (commit_obj.key !== null_ref)
                breadcrumb.append_breadcrumb(commit_obj.key);
            else
                breadcrumb.append_breadcrumb('HEAD');
        },

        start_file: function(file_obj) {
            var new_state = FileRenderer.create_from_data(file_obj);
            states.push( new_state );
            show_hide_toolbar_elements(new_state.gui_descr);
            breadcrumb.append_breadcrumb(file_obj.filename);
        },

        check_comparison: function(node_a, node_b) {
            var commit_a = $("[class*='branch_widget']", node_a);
            var commit_b = $("[class*='branch_widget']", node_b);
            var commit_count = (commit_a.length > 0 ? 1 : 0) +
                               (commit_b.length > 0 ? 1 : 0);

            if (commit_count == 2) {
                var b1 = commit_a.text().replace(/^\s+|\s+$/g, '');
                var b2 = commit_b.text().replace(/^\s+|\s+$/g, '');

                if (b1 == display_null_ref)
                    b1 = working_dir_request_token;

                if (b2 == display_null_ref)
                    b2 = working_dir_request_token;

                this.switch_commit_comp(b1, b2);
                return;
            }

            var file_a = $('> .file_widget', node_a);
            var file_b = $('> .file_widget', node_b);
            var file_count = (file_a.length > 0 ? 1 : 0) +
                             (file_b.length > 0 ? 1 : 0);

            if (commit_count > 0 && file_count > 0) {
                show_error({error: "Can't compare file and commit" });
                return;
            }

            if (file_count == 2) {
                var file1 = $('.path', file_a).text();
                var file2 = $('.path', file_b).text();
                var key1  = $('.key', file_a).text();
                var key2  = $('.key', file_b).text();

                if (key1 == display_null_ref)
                    key1 = working_dir_request_token;
                
                if (key2 == display_null_ref)
                    key2 = working_dir_request_token;

                this.switch_file_comp(key1, file1, key2, file2);
                return;
            }
        }
    };
})();

/**
 * @param {Element} node
 * @return {void}
 */
var remove_children = function(node) {
    while (node.hasChildNodes()) {
        node.removeChild(node.lastChild);
    }
};

/** @typedef ({way: Project.DiffChar, orig_idx: number, dest_idx: number, size: number}) */
var DiffInfo = {};

/** @typedef({way: Project.DiffChar, beg: number, end: number}) */
var DiffRange = {};

////////////////////////////////////////////////////////////
////  Diff handling
////////////////////////////////////////////////////////////
/** @namespace */
var DiffManipulator = (function () {
    "use strict";
    /**
     * @param {Element} n
     * @param {Array.<Element>} lst
     * @return {Element}
     */
    var append_all = function(n, lst) {
        var maxi = lst.length;

       for (var i = 0; i < maxi; i++) {
            n.appendChild(lst[i]);
        }

        n.appendChild(document.createTextNode('\n'));

        return n;
    }

    /**
     * @param {Array.<string>} kinds
     * @param {Array.<Array.<Element>>} nodeList
     * @return {Array.<Array.<Element>>}
     */
    var glob = function(kinds, nodeList) {
        var nodes = [];

        if (kinds.length === 0) return [];

        for (var j = 0; j < kinds.length; j++) {
            var node = document.createElement('div');
            node.setAttribute('class', kinds[j]);
            nodes.push(node);
            if (j > 0) nodes[j - 1].appendChild(node);
        }

        for (var i = 0; i < nodeList.length; i++) {
            append_all(nodes[nodes.length - 1], nodeList[i])
        }
        
        return [[ nodes[0] ]];
    };

    /** @type {Object.<Project.DiffChar, function(Array.<Array.<Element>>) : Array.<Array.<Element>>>} */
    var begs = {};
    begs[Project.DiffChar.DIFF_ADD] =
        function (n) { return glob(["diff_addition"], n); };
    begs[Project.DiffChar.DIFF_DEL] =
        function (n) { return glob(["diff_deletion"], n); };
    begs[Project.DiffChar.DIFF_DELADD] =
        function (n) { return glob(["diff_addition", "diff_deletion"], n); };
    begs[Project.DiffChar.DIFF_ADDDEL] =
        function (n) { return glob(["diff_deletion", "diff_addition"], n); };

    /**
     * @param {string} filename
     * @param {boolean} isLineNumberRequired
     * @param {string} data
     * @param {Array.<DiffRange>} diff
     * @param {Element} number_node
     * @param {Element} node
     */
    var generate_full_html = function (filename, isLineNumberRequired,
                                       data, diff, number_node, node) {
        var highlighter = TinySyntaxHighlighter.from_filename(isLineNumberRequired, filename);

        /** @type {Array.<string>} */
        var lines = data.split("\n");

        /** @type {number} */
        var diff_count = diff.length;

        /** @type {number} */
        var current_line = 0;

        /** @type {Array.<Array.<Element>>} */
        var diff_node;

        var add_number = function(n) {
            var node = document.createElement('span');
            node.setAttribute('class', 'syntax_line_number');
            node.appendChild(document.createTextNode(n.toString() + "\n"));

            number_node.appendChild(node);
        };

        remove_children(node);
        for (var i = 0; i < diff_count; i++) {
            while (current_line < diff[i].beg) {
                add_number(highlighter.current_line);
                append_all(node, highlighter.colorLine(lines[current_line]));
                current_line++;
            }

            var curr_diff = diff[i];
            var diff_nodes = [];

            for (var j = curr_diff.beg; j < curr_diff.end && current_line < lines.length; j++) {
                add_number(highlighter.current_line);
                diff_nodes.push( highlighter.colorLine(lines[current_line++]) );
            }

            diff_node = (begs[curr_diff.way])(diff_nodes);
            node.appendChild(diff_node[0][0]);
        }

        while (current_line < lines.length)
        {
            add_number(highlighter.current_line);
            append_all(node, highlighter.colorLine(lines[current_line]));
            current_line++;
        }
    }

    /** Generate an HTML representation of a diff and a file content.
     * @param {string} filename filename of the file, used for syntax detection
     * @param {number} contextSize
     * @param {boolean} isLineNumberRequired
     * @param {string} data content.
     * @param {Array.<DiffRange>} diff diff ranges
     * @param {Element} node
     */
    var generate_compact_html = function (filename, contextSize, isLineNumberRequired,
                                          data, diff, number_node, node) {
        var highlighter = TinySyntaxHighlighter.from_filename(isLineNumberRequired, filename);

        /** @type {Array.<string>} */
        var lines = data.split('\n');

        /** @type {Array.<Array.<Element>>} */
        var processed_lines = [];

        /** @type {number} */
        var last_outputted_line = -1;

        var add_number = function(n) {
            var node = document.createElement('span');
            node.setAttribute('class', 'syntax_line_number');
            node.appendChild(document.createTextNode(n.toString() + "\n"));

            number_node.appendChild(node);
        };

        remove_children(node);
        var i;
        for ( i = 0; i < diff.length; i++ )
        {
            var d = diff[i];
            
            // output the context before the diff
            var context_begin = Math.max(last_outputted_line + 1, d.beg - contextSize);

            // write an elipssiss if there is a deconnection
            if (context_begin > last_outputted_line + 1 && i != 0) {
                node.appendChild(document.createTextNode('...\n'));
                number_node.appendChild(document.createTextNode('...\n'));
            }

            highlighter.set_current_line_number(context_begin + 1);
            for ( var lineNum = context_begin; lineNum < d.beg; lineNum++ ) {
                add_number(highlighter.current_line);
                append_all(node, highlighter.colorLine(lines[lineNum]));
            }

            // output the real diff
            if (d.end - d.beg <= 1)
            {
                highlighter.set_current_line_number(d.beg + 1);
                add_number(highlighter.current_line);
                var new_node = begs[d.way]([highlighter.colorLine(lines[d.beg])]);
                node.appendChild(new_node[0][0]);
                new_node = undefined;
            }
            else
            {
                highlighter.set_current_line_number(d.beg + 1);
                processed_lines = [];

                for ( var lineNum = d.beg; lineNum < d.end; lineNum++ ) {
                    add_number(highlighter.current_line);
                    processed_lines.push(highlighter.colorLine(lines[lineNum]));
                }

                var new_node = (begs[d.way])(processed_lines);
                node.appendChild(new_node[0][0]);
                new_node = undefined;
            }

            var next_commit_begin = (i === diff.length - 1) ? lines.length - 1 : diff[i + 1].beg;
            var context_end = Math.min(d.end + contextSize, next_commit_begin);

            highlighter.set_current_line_number(d.end + 1);
            for ( var lineNum = d.end; lineNum < context_end; lineNum++ ) {
                add_number(highlighter.current_line);
                append_all(node, highlighter.colorLine(lines[lineNum]));
            }

            last_outputted_line = context_end;
        }
    }

    /** Combine two diff list into one list of edition
     * ranges.
     * @param {Array.<DiffCommand>} removings 
     * @param {Array.<DiffCommand>} addings   
     * @return {Array.<DiffRange>}
     */
    var calculate_fold_set = function (removings, addings) {
        /** @type {Array.<DiffRange>} */
        var ranges = [];

        /** @type {Array.<DiffRange>} */
        var lefts = [];

        /** @type {Array.<DiffRange>} */
        var rights = [];

        /** @type {DiffCommand} */
        var tempElem;

        for ( var i = 0; i < removings.length; i++ ) {
            tempElem = removings[i];
            lefts.push({ way: '-', beg: tempElem.orig_idx,
                        end: tempElem.orig_idx + tempElem.size});
            
        }

        for ( i = 0; i < addings.length; i++ ) {
            tempElem = addings[i];
            rights.push({ way: '+', beg: tempElem.dest_idx,
                          end: tempElem.dest_idx + tempElem.size});
        }

		/**
		 * @param a {string}
		 * @param b {string}
		 * @return {string}
		 */
        var combiner = function(a,b) {
            if (a === '+' && b === '-')
                { return '~' }
            else
                { return '!' }
        };

		/** @type {number} */
        var left_read_index = 0;

		/** @type {number} */
        var right_read_index = 0;

        /** @type {DiffRange} */
        var left = lefts[left_read_index];

        /** @type {DiffRange} */
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
            else if (left.beg < right.beg && left.end <= right.beg)  // ############
            {                                                       //                  ############
                ranges.push({ way: left.way, beg: left.beg, end: left.end });
                inc_left();
            }
            else if (left.beg < right.beg && left.end == right.end)   // ###############
            {                                                         //       #########
                ranges.push({ way: left.way, beg: left.beg, end: right.beg });
                ranges.push({ way: combiner(left.way, right.way)
                            , beg: right.beg, end: left.end });
                inc_left();
                inc_right();
            }
            else if (left.beg < right.beg && left.end < right.end)   // ###############
            {                                                        //       ##################
                ranges.push({ way: left.way, beg: left.beg, end: right.beg });
                ranges.push({ way: combiner(left.way, right.way)
                            , beg: right.beg, end: left.end });
                right.beg = left.end;
                inc_left();
            }
            else if (left.beg == right.beg) // ############
            {                               // ############
                if (left.end == right.end)
                {
                    ranges.push({ way: combiner(left.way, right.way)
                                , beg: left.beg, end: left.end });
                    inc_left();
                    inc_right();
                }
                else if (left.end < right.end ) // ############
                {                               // ##########################
                    ranges.push({ way: combiner(left.way, right.way)
                                , beg: left.beg, end: left.end });
                    right.beg = left.end;
                    inc_left();
                }
                else  // ##########################
                {     // #############
                    ranges.push({ way: combiner(left.way, right.way)
                                , beg: left.beg, end: right.end });
                    left.beg = right.end;
                    inc_right();
                }
            }
            else if (left.beg < right.beg && left.end > right.end)  // ############################
            {                                                       //        ############
                ranges.push({ way: left.way, beg: left.beg, end: right.beg });
                ranges.push({ way: combiner(left.way, right.way)
                            , beg: right.beg, end: right.end });
                left.beg = right.end;
                inc_right();
            }
            else
            {
                alert('Ergl error');
            }
        }

        while (left_read_index < lefts.length)
            { ranges.push(lefts[left_read_index++]); }

        while (right_read_index < rights.length)
            { ranges.push(rights[right_read_index++]);  }

        return ranges;
    }

    /** Keep only the diff information of adding (with a '+' way property)
     * @param {Array.<(DiffCommand|DiffRange)>} diffs
     * @return {Array.<(DiffCommand|DiffRange)>}
     */
    var filter_adds = function(diffs) {
        var ret = [];
        for (var i = 0; i < diffs.length; i++) {
            if (diffs[i].way === Project.DiffChar.DIFF_ADD)
                ret.push(diffs[i]);
        }
        return ret;
    }

    /** Keep only the diff information of adding (with a '-' way property)
     * @param {Array.<(DiffCommand|DiffRange)>} diffs
     * @return {Array.<(DiffCommand|DiffRange)>}
     */
    var filter_rems = function(diffs) {
        var ret = [];
        for (var i = 0; i < diffs.length; i++) {
            if (diffs[i].way === Project.DiffChar.DIFF_DEL)
                ret.push(diffs[i]);
        }
        return ret;
    }

    var to_diff_del_range = function(diff) {
        var ret = [];
        for (var i = 0; i < diff.length; i++) {
            ret.push({ way: diff[i].way, beg: diff[i].orig_idx, end: diff[i].orig_idx + diff[i].size });
        }
        return ret;
    }

    var to_diff_add_range = function(diff) {
        var ret = [];
        for (var i = 0; i < diff.length; i++) {
            ret.push({ way: diff[i].way, beg: diff[i].dest_idx, end: diff[i].dest_idx + diff[i].size });
        }
        return ret;
    }

    return {
        generateFullHtml:    generate_full_html,
        generateCompactHtml: generate_compact_html,
        filterAdds: filter_adds,
        filterRems: filter_rems,
        calculateFoldSet: calculate_fold_set,
        toDiffDelRange: to_diff_del_range,
        toDiffAddRange: to_diff_add_range 
    };
})();

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
////////////////////////////////////////////////////////////
////  Commit
////////////////////////////////////////////////////////////
/**
 * @param {Ref} key
 * @param {CommitDetail} data
 * @constructor
 */
var Commit = function(key, data) {
    "use strict";

    this.key = key === null_ref ? display_null_ref : key;
    this.is_key_real = key !== null_ref;
    this.commit_date = timestamp_to_string(data.timestamp);
    this.parents_sha = data.parents_sha;
    this.file_changes = data.file_changes;
    this.author = data.author
    this.message = data.message;

    var messages_lines = data.message.split('\n');
    var first_non_null = 0;

    while (messages_lines[first_non_null] === '')
        { first_non_null++; }
    
    this.head_message =
        html_encodize(messages_lines[first_non_null]);

    this.sub_message =
        html_encodize(messages_lines.slice(first_non_null + 1,
                                             messages_lines.length).join('\n')).replace(/\n/g, '<br/>');
    
    this.split_message =
        (html_encodize(data.message)).replace(/\n/g, '<br/>');
    this.tree_fetched = false;
    this.tree_opened = false;
    this.last_view_mode = Project.state.active_view_mode();
    this.focused_diff = 0;
    this.diff_nodes = [];

    if (data.hasOwnProperty('file_changes')) {
        this.fully_fetched = true;
        this.file_changes = data.file_changes;
    } else {
        this.fully_fetched = false;
        this.file_changes = [];
    }


    /** @type {!Object.<Project.DiffKind, function(json) : !jQuery>} */
    var kind_formater = {};
    kind_formater[Project.DiffKind.KIND_DELETION] = ich.commit_file;
    kind_formater[Project.DiffKind.KIND_ADDITION] = ich.commit_file;
    kind_formater[Project.DiffKind.KIND_MODIFICATION] = function(e) {
        var hl = TinySyntaxHighlighter.from_filename(false, e.name);

        /** @type {jQuery} */
        var rez_node = ich.commit_file_modification_detailed(e);

        /** @type {Element} */
        var code_node = rez_node.find('.syntax_highlighted')[0];

        /** @type {Element} */
        var number_node = rez_node.find('.line_number_column')[0];

        var curr_diff;
        var acc = ""
        var diff_node;

        var create_number_node = function(n) {
            var node = document.createElement('span');
            node.setAttribute('class', 'syntax_line_number');
            node.appendChild(document.createTextNode(n.toString() + "\n"));
            return node;
        };

		/** @type {function(string) : Element} */
        var div_node = function(kind) {
            var node = document.createElement('div');
            node.setAttribute('class', kind);
            return node;
        };

        if (e.binary) {
            return rez_node;
        }

        for ( var i = 0; i < e.diff.length; i++ )
        {
            var has_sub = false;

            curr_diff = e.diff[i];
			has_sub = (curr_diff.hasOwnProperty('sub') &&
					   curr_diff.sub.length > 0);

            if (curr_diff.way == '+') {
                diff_node = div_node("diff_addition");
                hl.set_current_line_number(curr_diff.dest_idx + 1);
            }
            else if (curr_diff.way == '-') {
                diff_node = div_node("diff_deletion");
                hl.set_current_line_number(curr_diff.dest_idx + 1);
            }
            else {
                diff_node = div_node("diff_context");
                hl.set_current_line_number(curr_diff.dest_idx + 1);
            }

            for ( var l = 0; l < curr_diff.data.length; l++ )
            {
                if (has_sub) {
                    hl.setPositionHighlight(curr_diff.sub[l]);
                }

                var lineNodes = hl.colorLine(curr_diff.data[l] + "\n");
                if (has_sub) {
                    hl.setPositionHighlight([]);
                }

                number_node.appendChild(create_number_node(hl.current_line - 1));

                for ( var node = 0; node < lineNodes.length; node++ )
                    diff_node.appendChild(lineNodes[node]);
            }

            code_node.appendChild(diff_node);

            if (i < e.diff.length - 1 &&
                curr_diff.dest_idx + curr_diff.size < e.diff[i + 1].dest_idx )
            {
                number_node.appendChild(document.createTextNode("...\n"));
                code_node.appendChild(document.createTextNode("...\n"));
            }

        }

        return rez_node;
    };

    this.move_up = function() {
        if (this.focused_diff === 0) {
            $(document).scrollTo(this.orig_node, 300, {offset: Project.state.chrome_scroll_offset()});
            return;
        }

        $(this.diff_nodes[this.focused_diff]).removeClass(sub_focus);

        this.focused_diff--;
        var new_node = this.diff_nodes[this.focused_diff];
        $(new_node).addClass(sub_focus);
        $(document).scrollTo(new_node, 300, {offset: Project.state.chrome_scroll_offset()});
    };

    this.move_down = function() {
        if ( this.focused_diff === this.diff_nodes.length - 1 )
            return;

        $(this.diff_nodes[this.focused_diff]).removeClass(sub_focus);

        this.focused_diff++;
        var new_node = this.diff_nodes[this.focused_diff];
        $(new_node).addClass(sub_focus);
        $(document).scrollTo(new_node, 300, {offset: Project.state.chrome_scroll_offset()});
    };

    this.send_message = function(msg) {
        if (msg.action === Project.GuiMessage.MOVE_UP)
            return this.move_up();
        else if (msg.action === Project.GuiMessage.MOVE_DOWN)
            return this.move_down();
        else if (msg.action === Project.GuiMessage.MOVE_INNER) {
            var file = this.file_changes[this.focused_diff];
            return Project.state.switch_file(file.name, file.hash, file.key);
        }
    };

	/**
	 * @param {Element} node
	 * @param {number} depth
	 * @param {string} tree_path
	 * @param {CommitTreeDiff} elem
	 * @return {Element}
	 */
    this.render_tree = function(node, depth, tree_path, elem) {

		/** @type {jQuery} */
        var new_node;
        var opened = false;

        elem.key = this.key;
        elem.full_path = (depth > 0) ? tree_path + "/" + elem.name
                                     : elem.name;

        if (elem.hasOwnProperty('children') && elem.children.length > 0)
        {
            if (depth == 0) {
                for ( var i = 0; i < elem.children.length; i++ )
                    this.render_tree(node, depth + 1, 
                                     elem.full_path, elem.children[i]);
                return node;
            }

            new_node = ich.tree_folder(elem);
            var child_node = $(".children", new_node);
            var button_indicator = $(".button", new_node);
            node.appendChild(new_node[0]);

            for ( var i = 0; i < elem.children.length; i++ )
                this.render_tree(child_node[0], depth + 1, 
                                 elem.full_path, elem.children[i]);

            child_node.animate({height: 'toggle'}, 0);

            $('> div.node_name', new_node).click(function( event ){
                event.stopPropagation();
                child_node.animate({height: 'toggle'}, 400);

                if (opened)
                    button_indicator.html('&#x25bc');
                else
                    button_indicator.html('&#x25b2');

                opened = !opened;
            });

        } else {
            new_node = ich.tree_elem(elem);
            make_draggable_elems( new_node );
            node.appendChild(new_node[0]);
        }

        return new_node;
    }

    this.fetch_tree = function() {
        var tree_node = $(".commit_tree", this.orig_node);
        var indicator = $(".btn_indicator", this.orig_node);

        if (this.tree_fetched) {
            indicator.html(this.tree_opened ? "&#x25bc;" : "&#x25b2;");
            tree_node.animate({height: 'toggle'});
            this.tree_opened = !this.tree_opened;
            return;
        }

        var this_obj = this;

        $.ajax({ url: '/ask_commit_tree/' + this.key,
            dataType: 'json',
            data: {},
            error: function() {
                show_error({error: 'Communication error with the server while fetching commit tree'});
            },
            success: 
				/** @param {(CommitTreeDiff|ErrorReturn)} data */
				function(data) {

                if (data['error']) { 
                    show_error( data );
                    return;
                }
                tree_node.animate({height: 'toggle'});
                this_obj.tree = data;
                this_obj.tree_fetched = true;
                this_obj.tree_opened = true;
                remove_children(tree_node[0]);
                this_obj.render_tree(tree_node[0], 0, "", data);
                tree_node.animate({height: 'toggle'});
                indicator.html("&#x25b2;");
            }
        });
    };

    this.create_dom = function() {
        var view_mode = Project.state.active_view_mode();

        if (view_mode == Project.ViewMode.VIEW_FULL) {
            this.orig_node = ich.commit_detailed(this);

            if (this.tree_fetched) {
                var tree_node = $(".commit_tree", this.orig_node);
                this.tree_opened = false;
                tree_node.animate({height: 'toggle'});
                this.render_tree(tree_node[0], 0, "", this.tree);
            }
        }
        else if (view_mode == Project.ViewMode.VIEW_COMPACT) {
            this.orig_node = ich.commit_compact(this);
        }

        return this.orig_node;
    };

    this.render_full = function() {
        this.diff_nodes = [];
        this.focused_diff = 0;

        for ( var change in this.file_changes ) {
            var e = this.file_changes[change];
            var kind = e['kind'];
            e.key = this.key;

            var new_node = ( kind_formater.hasOwnProperty(kind)
                           ? kind_formater[kind](e)
                           : ich.commit_file(e) );

            this.orig_node.append( new_node );
            this.diff_nodes.push( new_node );
        }

        make_draggable_elems( this.orig_node );
    };

    this.render_compact = function() {
        this.diff_ndoes = [];
    };

    this.render = function() {
    	var view_mode = Project.state.active_view_mode();

        if (view_mode == Project.ViewMode.VIEW_FULL) {
            this.render_full();
        } else if (view_mode == Project.ViewMode.VIEW_COMPACT) {
            this.render_compact();
        }
    };
    
    return this;
};

var CommitRenderer = (function() {
    "use strict";

    var fetch_commit =
		/**
		 * @param {Ref} id
		 * @param {function((CommitDetail|ErrorReturn))} f
		 */
				function( id, f ) {
        var this_obj = this;

        $.ajax({
            url: '/commit/' + id,
            dataType: 'json',
            data: {},
            success: f,
            error: function() {
                show_error({error: 'Communication error with the server while fetching commit'});
            }
        });
    }

    var insert_node = function( node ) {  
        var view_mode = Project.state.active_view_mode();

        if (view_mode == Project.ViewMode.VIEW_FULL) {
            $('.container').prepend(node);
        }
        else if (view_mode == Project.ViewMode.VIEW_COMPACT) {
            $('.container').append(node);
        }
    };

    var init = function(init_data) {
        var new_commit = new Commit(init_data.key, init_data);

        var new_node = new_commit.create_dom();
        insert_node(new_node);
        $(new_node).addClass(global_focus);
        new_commit.render();
        
        this.collection = [new_commit];
        this.keys[init_data.key] = new_commit;
    }

    /**
     * @constructor
     * @implements {ResultSet}
     */
    var init_methods = function() {
        this.keys = {};
        this.initial_view_mode = Project.state.active_view_mode();
        this.focused_index = 0;
        this.fetching = false;

        this.render_all = function() {
            if (Project.state.active_view_mode() !== this.initial_view_mode) {
                this.initial_view_mode = Project.state.active_view_mode();
                $('.container *').remove();
                this.create_all_dom();
            }

            for ( var i = 0; i < this.collection.length; i++ ) {
                this.collection[i].render();
            }
        };

        this.fetch_tree = function(key) {
            this.keys[key].fetch_tree();
        }

        this.move_left = function() {
            $(this.collection[this.focused_index].orig_node).removeClass(global_focus);

            if (this.focused_index === this.collection.length - 1) {
                this.fetch_previous();
            } else {
                this.focused_index++;
                var new_focused_node = this.collection[this.focused_index].orig_node;
                $(new_focused_node).addClass(global_focus);
                $(document).scrollTo(new_focused_node, 200, {offset: Project.state.chrome_scroll_offset()});
            }
        };

        this.move_right = function() {
            if (this.focused_index === 0) return;
            $(this.collection[this.focused_index].orig_node).removeClass(global_focus);
            this.focused_index--;
            var new_focused_node = this.collection[this.focused_index].orig_node;
            $(new_focused_node).addClass(global_focus);
            $(document).scrollTo(new_focused_node, 200, {offset: Project.state.chrome_scroll_offset()});
        };


        this.send_message = function( msg ) {
            if (msg.action === Project.GuiMessage.FETCH_TREE)
                return this.fetch_tree(msg.key);
            else if (msg.action === Project.GuiMessage.MOVE_LEFT)
                return this.move_left();
            else if (msg.action === Project.GuiMessage.MOVE_RIGHT)
                return this.move_right();
            else if (msg.action === Project.GuiMessage.MOVE_FIRST) {
                this.focused_index = Math.max(0, this.collection.length - 2);
                return this.move_left();
            }
            else if (msg.action === Project.GuiMessage.MOVE_LAST) {
                this.focused_index = 1;
                return this.move_right();
            }
            else
                return this.collection[this.focused_index].send_message(msg);
        };

        this.create_all_dom = function() {
            for ( var i = 0; i < this.collection.length; i++ ) {
                insert_node(this.collection[i].create_dom());
            }
        };

        this.gui_descr = { compact_view: true, fetch_previous: true
                         , context_size: false, syntax_toggle: false };

        this.fetch_previous = function() {
            var this_obj = this;
            var parents = this.collection[this.collection.length - 1].parents_sha;
            var prev_id = parents.length > 0 ? parents[0] : null_ref;

            if (this_obj.fetching)
                return;

            this_obj.fetching = true;

            fetch_commit(prev_id, function(data) {
                if (data['error']) {
                    show_error( data );
                    return;
                }

                var new_commit = new Commit(data.key, data);

                var new_node = new_commit.create_dom();
                insert_node(new_node);
                new_node.animate({'width':'toggle'}, 0);
                new_commit.render();
                new_node.animate({'width':'toggle'}, Project.state.apparition_duration());

                this_obj.collection.push(new_commit);
                this_obj.keys[data.key] = new_commit;
                this_obj.move_left();

                this_obj.fetching = false;
            });
        }
    }

    return {
        create_from_data: function(init_data) { 
            var rez = new init_methods();
            init.call(rez, init_data);
            return rez;
        },

        create_from_arg: function(key) {
            var rez = new init_methods();

            fetch_commit(key, function(data) { 
            	if (data['error']) {
					show_error(data.error);
					return undefined
				}
                init.call(rez, data);
            });

            return rez;
        }
    };
})();

/**
 * @param {ParentFile} data
 * @constructor
 */
var FileBlob = function (data) {
    "use strict";

    /** @type {function(string, Array.<string>) : Array.<Element>} */
    var add_syntax_coloration = function( filename, lines )
    {
        /** @type {Array.<Element>} */
        var ret = [];
        var highlighter = TinySyntaxHighlighter.from_filename(true, filename);

        /** @type {number} */
        var i;

        for ( i = 0; i < lines.length; i++ )
            ret.push(highlighter.colorLine(lines[i]));

        return ret;
    }

    /** @type {boolean} */
    this.detail_fetched = false;

    /** @type {string} */
    this.file = data.filename;

    this.binary = data.binary;
    this.diff = data.diff;
    this.data = data.data;

    this.commit_date = timestamp_to_string(data.timestamp);

    /** @type {ref} */
    this.filekey = data.filekey;

    /** @type {ref} */
    this.key = data.key;

    /** @type {string} */
    this.message = data.message;
    this.parent_commit = data.parent_commit;
    this.path = data.path;

    for (var i = 0; i < this.path.length; i++) {
        this.path[i].commit_date = timestamp_to_string(this.path[i].timestamp);

        this.path[i].splited_message =
            html_encodize(this.path[i].message).replace("\n", "<br/>");
    }

    this.ellipsis_size = this.path.length - 15;


    this.create_dom_details = function(node) {
        var detail_node = $('.commit_detail', node);

        for ( var i = 0; i < this.details.length; i++ )
        {
            var e = this.details[i];
            e.key = this.key;
            detail_node.append(ich.commit_file(e));
        }

        make_draggable_elems(node);
    };

    this.toggle_detail_pane = function(node) {

        var detail = $('.commit_detail', node);
        var btn_node = $('.more_info', node);
        var btn_text = btn_node.text();

        if (btn_text[0] == '▼')
            btn_node.html("&#x25b2");
        else
            btn_node.html("&#x25bc");

        detail.animate({height: 'toggle'}, Project.state.apparition_duration);
    };

    this.fetch_details = function() {
        if (this.detail_fetched)
        {
            this.toggle_detail_pane(this.orig_node);
            return;
        }

        var this_obj = this;
        $.ajax({
            url: '/ask_commit/' + this.key,
            dataType: 'json',
            data: {},
            error: function() {
                show_error({error: 'Communication error with the server while fetching commit details'});
            },
            success: 
            
				/** @param {ParentFile|ErrorReturn} data */
				function(data) {
                if (data === null) {
                    show_error({error: 'Communication error with the server'});
                    return;
                }

                if (data['error']) { 
                    show_error( data );
                    return;
                }

                this_obj.detail_fetched = true;

                var kind_formater = {
                    'modification': ich.commit_file_modification,
                    'addition':ich.commit_file,
                    'deletion':ich.commit_file
                };

                this_obj.details = data;

                var detail_node = $('.commit_detail', this_obj.orig_node);
                detail_node.animate({height: 'toggle'}, 0);
                this_obj.create_dom_details(this_obj.orig_node);
                this_obj.toggle_detail_pane(this_obj.orig_node);
            }
        });
    };

    this.render_file_data = function(next_diff, number_node, node) {
        var filename = this.file;
        var diff     = this.diff;
        var data     = this.data;

        var rems = DiffManipulator.filterRems(next_diff);
        var adds = DiffManipulator.filterAdds(diff);
        var ranges = DiffManipulator.calculateFoldSet(rems, adds);

        var clean_cr_lf_data = data.replace(/\r/g, '');

        if (!this.binary) {
            if (Project.state.active_view_mode() === Project.ViewMode.VIEW_COMPACT)
                DiffManipulator.generateCompactHtml(filename, Project.state.active_context_size(),
                                                    false, clean_cr_lf_data, ranges,
                                                    number_node[0], node[0]);
            else // render full
                DiffManipulator.generateFullHtml(filename, false, clean_cr_lf_data, ranges,
                                                 number_node[0], node[0]);
        } else {
            node[0].appendChild(document.createTextNode("Binary element"));
        }

    }


    this.create_dom = function() {
        this.short_message = this.message.split("\n")[0];
        this.splited_message = html_encodize(this.message).replace("\n", "<br/>");

        var path_length = this.path.length;
        var maximum_path_length = 15;

        if (path_length > maximum_path_length)
        {
            this.path_beg = this.path.slice(0, maximum_path_length / 2);
            this.path_end = this.path.slice(this.path.length - maximum_path_length / 2,
                                            this.path.length - 1);
            this.orig_node = ich.commitfile_huge_path(this);
        }
        else
            this.orig_node = ich.commitfile(this);

        if (this.detail_fetched)
        {
            this.create_dom_details(this.orig_node);
            this.toggle_detail_pane(this.orig_node);
            $('.more_info', this.orig_node).html("&#x25bc");
        }

        make_draggable_elems(this.orig_node);
        return this.orig_node;
    }

    this.render = function(next_diff) {
        var render_node = $('.syntax_highlighted', this.orig_node);
        var number_node = $('.line_number_column', this.orig_node);

        render_node.detach();
        render_node.empty();
        number_node.empty();
        this.render_file_data(next_diff, number_node, render_node);
        render_node.appendTo($('table td:last', this.orig_node));
    }

    return this;
};


/**
 * @namespace
 */
var FileRenderer = (function() {
    "use strict";

    var fetch_file = function(file, commit, filekey, f) {
        var request = '/ask_parent/' + commit;

        if (file[0] == '/') request += file;
        else request += '/' + file;

        $.ajax({ url: request, dataType: 'json',
                 error: function() {
                     show_error({error: 'Communication error with the server while fetching file'});
                 },
                 success: f });
    };

    var insert_node = function(node) {
        $(".container").prepend( node );
    };

    var init = function(init_data) {
        var init_file = new FileBlob(init_data);

        this.collection = [init_file];
        this.keys[init_file.key] = init_file;

        var new_node = init_file.create_dom();
        insert_node( new_node );
        $(new_node).addClass(global_focus);
        init_file.render([]);

        return this;
    }

    /** 
     * @constructor
     * @implements {ResultSet}
     */
    var init_methods = function() {

        /** @type {Array.<FileBlob>} */
        this.collection = [];
        this.keys = {};
        this.focused_index = 0;
        this.fetching = false;
        this.gui_descr = { compact_view: true, fetch_previous: true
                         , context_size: true, syntax_toggle: false };

        this.create_all_dom = function() {
            for ( var i = this.collection.length - 1; i >= 0; i-- ) {
                insert_node(this.collection[i].create_dom());
            }
        };


        this.render_all = function() {
            var i;

            for ( i = 0; i < this.collection.length - 1; i++ ) {
                this.collection[i].render(this.collection[i + 1].diff);
            }

            this.collection[i].render([]);
        };

        this.fetch_details = function(commit_id) {
            this.keys[commit_id].fetch_details();
        };

        this.move_left = function() {
            $(this.collection[this.focused_index].orig_node).removeClass(global_focus);

            if (this.focused_index === 0) {
                this.fetch_previous();
                return;
            }

            this.focused_index--;

            var new_focused_node = this.collection[this.focused_index].orig_node;
            $(new_focused_node).addClass(global_focus);
            $(document).scrollTo(new_focused_node, 200, {offset: Project.state.chrome_scroll_offset()});
        };

        this.move_right = function() {
            if (this.focused_index === this.collection.length - 1)
                return;

            $(this.collection[this.focused_index].orig_node).removeClass(global_focus);
            this.focused_index++;

            var new_focused_node = this.collection[this.focused_index].orig_node;
            $(new_focused_node).addClass(global_focus);
            $(document).scrollTo(new_focused_node, 200, {offset: Project.state.chrome_scroll_offset()});
        };

        this.send_message = function( msg ) {
            if (msg.action === Project.GuiMessage.FETCH_DETAIL)
                return this.fetch_details(msg.key);
            else if (msg.action === Project.GuiMessage.MOVE_LEFT)
                return this.move_left();
            else if (msg.action === Project.GuiMessage.MOVE_RIGHT)
                return this.move_right();
        };

        this.fetch_previous = function() {
            var last_commit = this.collection[0];
            var this_obj = this;

            if (this.fetching) return;

            this_obj.fetching = true;

            fetch_file(last_commit.file, last_commit.parent_commit,
                       last_commit.filekey, function(data) {
                                
                if (data === null) {
                    show_error({error: 'Communication error with the server'});
                    return;
                }

                if (data['error']) { 
                    show_error( data );
                    return;
                }

                var new_commit = new FileBlob(data);

                var node = new_commit.create_dom();
                insert_node(node);

                this_obj.collection.unshift( new_commit );
                this_obj.focused_index++;
                this_obj.keys[new_commit.key] = new_commit;
                node.animate({'width': 'toggle'}, 0);
                this_obj.collection[0].render(this_obj.collection[1].diff);
                node.animate({'width': 'toggle'}, Project.state.apparition_duration() * 2);
                this_obj.move_left();

                this_obj.fetching = false;
            });
        };
        return this;
    };

    return {
        create_from_data: function(init_data) { 
            var inited = new init_methods();
            return init.call(inited, init_data);
        },

        create_from_arg: function(file, filekey, commit) {
            var rez = new init_methods();

            fetch_file(file, commit, filekey, function(data) { 
                data.file = file;
                init.call(rez, data);
            });

            return rez;
        }
    };
})();

var CommitComparer = (function() {
    var init_methods = function() {

        this.fetch_previous = function() {
            show_error({error: 'Does not exists in this mode'});
        };

        this.render_all = function() {
            /* nothing */
            if (this.last_comparison)
                this.last_comparison.render();
        };

        this.refresh_diff = function() {
            var this_obj = this;

            $.ajax({  
                url:'/compare_branches/' + encodeURIComponent(this.branch_a)
                                   + '/' + encodeURIComponent(this.branch_b),
                dataType: 'json',
                data: {},
                error: function() {
                    show_error({error: 'Communication error with server while comparing branches'}); },
                success: function(data) {
                    if (data['error']) { 
                        show_error( data );
                        return;
                    }

                    this_obj.last_comparison = new Commit(data.key, data);
                    this_obj.create_all_dom();
                    this_obj.render_all();
                }
            });
        }

        this.create_all_dom = function() {
            $('.container').append(this.last_comparison.create_dom());
        };

        this.send_message = function( msg ) {
            if (this.last_comparison)
                this.last_comparison.send_message(msg);
        };

        this.gui_descr = { compact_view: false, fetch_previous: false
                         , context_size: false, syntax_toggle: false };
    }

    return {
        create_from_args: function(b1, b2) {
            var created = new init_methods();
            created.branch_a = b1;
            created.branch_b = b2;
            created.refresh_diff();

            return created;
        }
    };
})();

var FileComparer = (function() {
    var init_methods = function() {

        this.fetch_previous = function() {
            show_error({error: 'Does not exists in this mode'});
        };

        this.render_all = function() {
            var rems = DiffManipulator.toDiffDelRange( DiffManipulator.filterRems(this.data.diff) );
            var adds = DiffManipulator.toDiffAddRange( DiffManipulator.filterAdds(this.data.diff) );
            var number_node = $('.line_number_column');
            var node = $('.syntax_highlighted');

            number_node.children().remove();

            if (Project.state.active_view_mode() === Project.ViewMode.VIEW_COMPACT) {
                DiffManipulator.generateCompactHtml(this.file1, Project.state.active_context_size(),
                                                    false, this.data.data_orig, rems,
                                                    number_node[0], node[0]);

                DiffManipulator.generateCompactHtml(this.file2, Project.state.active_context_size(),
                                                    false, this.data.data_dest, adds,
                                                    number_node[1], node[1]);
            }
            else { // render full
                DiffManipulator.generateFullHtml(this.file1, false, this.data.data_orig, rems,
                                                 number_node[0], node[0]);

                DiffManipulator.generateFullHtml(this.file2, false, this.data.data_dest, adds,
                                                 number_node[1], node[1]);
            }
        };

        this.refresh_diff = function() {
            var this_obj = this;
            var url = ('/compare_files/' + encodeURIComponent(this.key1)
                                   + '/' + encodeURIComponent(this.file1.slice(1))
                                   + '/' + encodeURIComponent(this.key2)
                                   + this.file2);

            $.ajax({  
                url: url,
                dataType: 'json',
                data: {},
                error: function() {
                    show_error({error: 'Communication error with server while comparing files'}); },
                success: function(data) {
                    if (data['error']) { 
                        show_error( data );
                        return;
                    }

                    this_obj.data = data;
                    this_obj.create_all_dom();
                    this_obj.render_all();
                }
            });
        }

        this.create_all_dom = function() {
            $('.container').append(ich.compare_files(this));
        };

        this.send_message = function( msg ) {};

        this.gui_descr = { compact_view: true, fetch_previous: false
                         , context_size: true, syntax_toggle: false };
    }

    return {
        create_from_args: function(key1, file1, key2, file2) {
            var created = new init_methods();
            created.key1 = key1;
            created.file1 = file1;
            created.key2 = key2;
            created.file2 = file2;
            created.refresh_diff();

            return created;
        }
    };
})();

var BlameShower = (function() {
    "use strict";

    var returnGenerator = function(range) {
        var n = range.size - 1;
        var ret = '';
        var messageLines = range.tag.message.split('\n');
        var i = 0;

        var splitedMessageLines = [];
        for (var i = 0; i < messageLines.length; i++) {
            var sub = messageLines[i].match(/.{1,30}/g);

            if (!sub) continue;
            if (sub.length > 1) {
                for (var j = 0; j < sub.length; j++) {
                    splitedMessageLines.push(sub[j] + " ...");
                }
            }
            else splitedMessageLines.push(sub[0]);
        }

        if (n > 0) {
            ret += '\n(' + range.tag.author + ')';
        }

        for (i = 1; i - 1 < splitedMessageLines.length && i < n; i++) {
            ret += '\n' + splitedMessageLines[i - 1];
        }

        for (i = i; i < n; i++) ret += '\n&nbsp;';
        return ret;
    }

    var init = function(data) {
        this.data = data;

        var ranges = this.data.ranges;

        for (var i = 0; i < ranges.length; i++) {
            ranges[i].padd_string = returnGenerator(ranges[i]);
        }

        this.create_all_dom();

        this.render_all();
    };

    var fetch = function( obj, key, file ) {
        var this_obj = obj;
        if (file[0] != '/') file = '/' + file;

        $.ajax({ url: '/blame/' + encodeURIComponent(key) + file,
            dataType: 'json',
            data: {},
            error: function() {
                show_error({error: 'Communication error with the server while fetching blame'});
            },
            success: function(data) {
                if (data['error']) { 
                    show_error( data );
                    return;
                }

                init.call(this_obj, data);
            }
        });
    }

    var init_methods = function() {
        this.set_backgrounds_colors = function() {
            var ranges = this.data.ranges;

            var nodes = $('.blame_range', this.orig_node);
            var date_interval = this.data.latest - this.data.earliest;

            for (var i = 0; i < ranges.length; i++) {
                var range = ranges[i];
                var zeroToOne = (range.tag.timestamp - this.data.earliest) / date_interval;
                var newColor = color_lerp(blame_gradient.beg, blame_gradient.end, zeroToOne);

                $(nodes[i]).css('background-color', newColor);
                $(nodes[i]).hover(function() {
                    $(this).addClass('blame_hover');
                }, function() {
                    $(this).removeClass('blame_hover');
                });
            }
        }

        this.fetch_previous = function() {
            show_error({error: 'Does not exists in this mode'});
        };

        this.render_all = function() {
            var render_node = $('.syntax_highlighted', this.orig_node);
            var number_node = $('.line_number_column', this.orig_node);
            var clean_cr_lf_data = this.data.data.replace(/\r/g, '');

            DiffManipulator.generateFullHtml(this.data.filename, false,
                                             clean_cr_lf_data, [],
                                             number_node[0], render_node[0]);
            this.set_backgrounds_colors();
        };

        this.create_all_dom = function() {
            this.orig_node = ich.blamefile(this.data);
            $('.container').append(this.orig_node);
        };

        this.send_message = function( msg ) {
        };

        this.gui_descr = { compact_view: false, fetch_previous: false
                         , context_size: false, syntax_toggle: false };
    }

    return {
        create_from_data: function(data) {
            var created = new init_methods();
            init.call(created, data);
            return created;
        },

        create_from_arg: function(key, file) {
            var created = new init_methods();
            fetch( created, key, file );
            return created;
        }
    };
})();

function fetch_branch_list() {
    $.ajax({
        url:'/branches_list', dataType: 'json',
        data: {},
        error: function() {
            show_error({error: 'Communication error with server while fetching branches'}); },
        success: function(data) {
            if (data['error']) { 
                show_error( data );
                return;
            }

            var new_node = ich.branch_list({ref_list: data});
            $('body').append(new_node);
            make_draggable_elems(new_node);
            setup_branch_toggle();
        }
    });
}

function show_error( data )
{
    $('.message_carret').html( data.error )
}


function leave_server()
    { $.ajax( {url:"/quit", async:false} ); }

function setup_global_drop()
{
    var zone_a = $('.global_compare_recipient_a');
    var zone_b = $('.global_compare_recipient_b');

    zone_a.droppable({
        drop: function(evt, ui) {
            zone_a.children().remove();
            zone_a.append($(ui.draggable).clone());
            $(ui.helper).remove();
            Project.state.check_comparison(zone_a, zone_b);
        }
    });

    zone_b.droppable({
        drop: function(evt, ui) {
            zone_b.children().remove();
            zone_b.append($(ui.draggable).clone());
            $(ui.helper).remove();
            Project.state.check_comparison(zone_a, zone_b);
        }
    });
}

function setup_branch_toggle() {  
    $('.branch_list .list').animate({width: 'toggle'}, 0);

    var is_opened = false;

    $('.branch_list .toggler').click( function( event ){
        $('.branch_list .list').animate({width: 'toggle'}, 100);
        if (is_opened) {
            $('.branch_list .toggler div').html('&#x25bc; Branches');
        } else {
            $('.branch_list .toggler div').html('&#x25b2 Branches');
        }

        is_opened = !is_opened;
    })
}

ich.grabTemplates();

$(document).ready(function() {
    fetch_branch_list();
    setup_global_drop();
});

//////////////////////////////////////////////////////////////////////////
////            Keyboard shortcuts
//////////////////////////////////////////////////////////////////////////
$(document).bind('keydown', 'h', function(){
    Project.state.send_state_message({action: Project.GuiMessage.MOVE_LEFT});
});

$(document).bind('keydown', 'j', function(){
    Project.state.send_state_message({action: Project.GuiMessage.MOVE_DOWN});
});

$(document).bind('keydown', 'k', function(){
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

$(document).bind('keydown', 'ctrl+left', function() {
    breadcrumb.go_backward();
});

$(document).bind('keydown', 'ctrl+right', function() {
    breadcrumb.go_forward();
});

