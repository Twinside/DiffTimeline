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

        remove_children(number_node);
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

            highlighter.reset_context();
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

    /**
     * @param {number} line
     * @param {Array.<DiffCommand>} diffs
     * @return {number}
     */
    var to_prev_diff = function(line, diffs) {
        var i = 0;
        var offset = 0;
        var diff;

        var add = '+';
        var del = '-';
        var neutral = '=';

        for (i = 0; i < diffs.length; i++) {
            diff = diffs[i];

            if (diff.way === neutral)
                continue;

            if (line < diff.dest_idx)
                break;

            if (diff.way === del) {
                offset += diff.size;
            }
            else if (diff.way === add) {
                if (line < diff.dest_idx + diff.size) {
                    return diff.orig_idx;
                } else {
                    offset -= diff.size;
                }
            }
        }

        return line + offset;
    };

    /**
     * @param {number} line
     * @param {Array.<DiffCommand>} diffs
     * @return {number}
     */
    var to_next_diff = function(line, diffs) {
        var i = 0;
        var offset = 0;
        var diff;

        var add = '+';
        var del = '-';
        var neutral = '=';

        for (i = 0; i < diffs.length; i++) {
            diff = diffs[i];

            if (diff.way === neutral)
                continue;

            if (line < diff.orig_idx)
                break;

            if (diff.way === add) {
                offset += diff.size;
            }
            else if (diff.way === del) {
                if (line < diff.orig_idx + diff.size) {
                    return diff.dest_idx;
                } else {
                    offset -= diff.size;
                }
            }
        }

        return line + offset;
    };

    return {
        generateFullHtml:    generate_full_html,
        generateCompactHtml: generate_compact_html,
        filterAdds: filter_adds,
        filterRems: filter_rems,
        calculateFoldSet: calculate_fold_set,
        toDiffDelRange: to_diff_del_range,
        toDiffAddRange: to_diff_add_range,

        toNextDiff: to_next_diff,
        toPrevDiff: to_prev_diff
    };
})();

