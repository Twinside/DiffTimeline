/// <reference path="project.ts" />
/// <reference path="resultset.ts" />
/// <reference path="difftimeline.extern.ts" />
/// <reference path="../tinysyntaxhighlighter.ts" />

namespace DiffManipulator {

    function append_all(n : Node, lst : Node[]) : Node{
        const maxi = lst.length;

        for (var i = 0; i < maxi; i++) {
            n.appendChild(lst[i]);
        }

        n.appendChild(document.createTextNode("\n"));

        return n;
    }

    function glob(kinds : string[], nodeList : Node[][]) : Node[][] {
        const nodes: Node[] = [];

        if (kinds.length === 0) return [];

        for (var j = 0; j < kinds.length; j++) {
            const node = document.createElement('div');
            node.setAttribute("class", kinds[j]);
            nodes.push(node);
            if (j > 0) nodes[j - 1].appendChild(node);
        }

        for (let i = 0; i < nodeList.length; i++) {
            append_all(nodes[nodes.length - 1], nodeList[i])
        }
        
        return [[ nodes[0] ]];
    }

    class BegAssoc {
        public constructor() {
            this[Project.DiffChar.DIFF_ADD] =
                n => { return glob(["diff_addition"], n); };
            this[Project.DiffChar.DIFF_DEL] =
                n => { return glob(["diff_deletion"], n); };
            this[Project.DiffChar.DIFF_DELADD] =
                n => { return glob(["diff_addition", "diff_deletion"], n); };
            this[Project.DiffChar.DIFF_ADDDEL] =
                n => { return glob(["diff_deletion", "diff_addition"], n); };
        }
        [ix: string]: (nodes: Node[][]) => Node[][];
    }

    var begs : BegAssoc = new BegAssoc();

    export function generateFullHtml(filename : string,
								isLineNumberRequired : boolean,
                                data : string,
								diff : DiffRange[],
								number_node : Element,
								node : Element) {
        const highlighter = TinySyntaxHighlighter.from_filename(isLineNumberRequired, filename);

        const lines: string[] = data.split("\n");
        const diff_count = diff.length;
        let current_line = 0;

        let diff_node: Node[][];

        const add_number = (n : number) => {
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
            var diff_nodes : Node[][]= [];

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

    export function generateCompactHtml (filename : string,
                                         contextSize : number,
                                         isLineNumberRequired : boolean,
                                         data : string,
                                         diff : DiffRange[],
                                         number_node : Node,
                                         node : Node) {
        var highlighter = TinySyntaxHighlighter.from_filename(isLineNumberRequired, filename);
        var lines : string[] = data.split('\n');
        var processed_lines : Node[][] = [];
        var last_outputted_line = -1;

        var add_number = function(n : number) {
            var node = document.createElement('span');
            node.setAttribute('class', 'syntax_line_number');
            node.appendChild(document.createTextNode(n.toString() + "\n"));

            number_node.appendChild(node);
        };

        remove_children(node);
        for ( let i = 0; i < diff.length; i++ )
        {
            let d = diff[i];
            
            // output the context before the diff
            let context_begin = Math.max(last_outputted_line + 1, d.beg - contextSize);

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

                for ( let lineNum : number = d.beg; lineNum < d.end; lineNum++ ) {
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
            for ( let lineNum = d.end; lineNum < context_end; lineNum++ ) {
                add_number(highlighter.current_line);
                append_all(node, highlighter.colorLine(lines[lineNum]));
            }

            last_outputted_line = context_end;
        }
    }

    /** Combine two diff list into one list of edition
     * ranges.
     */
    export function calculateFoldSet(removings : DiffCommand[],
                                     addings : DiffCommand[]) : DiffRange[] {
        let ranges : DiffRange[] = [];
        let lefts : DiffRange[] = [];
        let rights : DiffRange[] = [];
        let tempElem : DiffCommand;

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

        let combiner = (a : string, b : string) : string => {
            if (a === '+' && b === '-')
                { return '~' }
            else
                { return '!' }
        };

        let left_read_index : number = 0;
        let right_read_index : number = 0;

        let left : DiffRange = lefts[left_read_index];
        let right : DiffRange = rights[right_read_index];

        let swapArrays = () => {
            let swap_idx = left_read_index;
            left_read_index = right_read_index;
            right_read_index = swap_idx;

            let swap_array = lefts;
            lefts = rights;
            rights = swap_array;

            let swap_obj = left;
            left = right;
            right = swap_obj;
        };

        let inc_right = () => {
            right_read_index++;
            if (right_read_index < rights.length)
                right = rights[right_read_index];
        }

        let inc_left = () => {
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
     */
    export function filterAdds(diffs : (DiffCommand | DiffRange)[])
        : (DiffCommand | DiffRange)[] {
        var ret : (DiffCommand | DiffRange)[] = [];
        for (var i = 0; i < diffs.length; i++) {
            if (diffs[i].way === Project.DiffChar.DIFF_ADD)
                ret.push(diffs[i]);
        }
        return ret;
    }

    /** Keep only the diff information of adding (with a '-' way property)
     */
    export function filterRems(diffs : (DiffCommand|DiffRange)[])
        : (DiffCommand|DiffRange)[]
    {
        var ret : (DiffCommand|DiffRange)[] = [];
        for (var i = 0; i < diffs.length; i++) {
            if (diffs[i].way === Project.DiffChar.DIFF_DEL)
                ret.push(diffs[i]);
        }
        return ret;
    }

    export function toDiffDelRange(diff : DiffCommand[]) : DiffRange[] {
        let ret : DiffRange[]= [];
        for (let i = 0; i < diff.length; i++) {
            ret.push({ way: diff[i].way, beg: diff[i].orig_idx, end: diff[i].orig_idx + diff[i].size });
        }
        return ret;
    }

    export function toDiffAddRange(diff : DiffCommand[]) : DiffRange[] {
        let ret : DiffRange[]= [];
        for (let i = 0; i < diff.length; i++) {
            ret.push({ way: diff[i].way, beg: diff[i].dest_idx, end: diff[i].dest_idx + diff[i].size });
        }
        return ret;
    }

    export function toPrevDiff(line : number, diffs : DiffCommand[]) : number {
        let i = 0;
        let offset = 0;
        let diff : DiffCommand;

        const add = '+';
        const del = '-';
        const neutral = '=';

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
    }

    export function toNextDiff(line : number, diffs : DiffCommand[]) : number {
        let i = 0;
        let offset = 0;
        let diff : DiffCommand;

        const add = '+';
        const del = '-';
        const neutral = '=';

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
}
