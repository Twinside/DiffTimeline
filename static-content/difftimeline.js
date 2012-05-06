/////////////////////////////////////////////////////////////////////
//              Initial state
/////////////////////////////////////////////////////////////////////
var breadcrumb = (function() {
    var count = 0;
    var current_index = 0;

    return {
        append_breadcrumb: function( name ) {
            if (current_index < count - 1)
            {
                $('#breadcrumb > span').slice(current_index + 1).remove();
                count = current_index + 1;
            }

            $('#breadcrumb').append(ich.breadcrumbelem({name:name, id:count}));
            current_index = count++;
        },

        click_index: function( idx ) {
            current_index = idx;
            application_state.jump_context(idx);
        }
    };
})();

/**
 * @constructor
 */
var application_state = (function () {  
    var view_mode = 'full';
    var apply_syntax_coloration = true;
    var context_size = 2;
    var max_commit_delta_show = 15;
    var states = [];
    var forward_states = [];
    var commit_delta_margin = 6;
    var apparition_duration = 750;
    var apparition_pane_duration = 500;

    var btn_toggle_text = {
        full: '&#x25bc;<br/>&#x25b2;',
        compact:  '&#x25b2;<br/>&#x25bc;'
    };


    return {
        active_view_mode: function() { return view_mode; },
        active_context_size: function() { return context_size; },
        apparition_duration: function() { return apparition_duration; },

        send_state_message: function(message) {
            var last_path = states[ states.length - 1 ];
            last_path.send_message(message);
        },

        create_all_dom: function() {
            var last_path = states[ states.length - 1 ];
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

        switch_file: function(file, fkey, start_commit) {
            this.clear_display();
            states.push(
                FileRenderer.create_from_arg(file, fkey, start_commit));
            breadcrumb.append_breadcrumb(file);
        },

        switch_commit: function(id) {
            this.clear_display();
            states.push(new CommitRenderer.create_from_arg(id));
            breadcrumb.append_breadcrumb(id);
        },

        push_last_commit: function(v) {
            states[ states.length - 1 ].val.unshift(v);
        },

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
            $('.toolbar div textarea').text(context_size);

            if (view_mode === 'compact') { this.render_all(); }
        },

        decrease_context_size: function() {
            context_size = Math.max(0, context_size - 1);
            $('.toolbar div textarea').text(context_size);

            if (view_mode === 'compact') { this.render_all(); }
        },

        toggle_diff_full: function() {
            var ranges = null;

            if (view_mode === 'full') view_mode = 'compact';
            else view_mode = 'full';

            $('.btn_toggleview').html(btn_toggle_text[view_mode]);
            this.render_all();
        },

        start_commit: function(commit_obj) {
            states.push( CommitRenderer.create_from_data(commit_obj) );
            breadcrumb.append_breadcrumb(commit_obj.key);
        },

        start_file: function(file_obj) {
            states.push( FileRenderer.create_from_data(file_obj) );
            breadcrumb.append_breadcrumb(file_obj.file);
        }
    };
})();

////////////////////////////////////////////////////////////
////  Diff handling
////////////////////////////////////////////////////////////
/**
 * @constructor
 */
var DiffManipulator = (function () {
    /** Generate an HTML representation of a diff and a file content.
     * @param filename filename of the file, used for syntax detection
     * @param contextSize Int
     * @param isLineNumberRequired {Bool}
     * @param data {String} file content.
     * @param diff [{way:String, beg:Int, end:Int}] diff ranges
     */
    var generate_compact_html = function (filename, contextSize, isLineNumberRequired, data, diff) {
        var highlighter = TinySyntaxHighlighter.from_filename(isLineNumberRequired, filename);
        var lines = data.split("\n");
        var begs = {
            '+': '<div class="diff_addition">',
            '-': '<div class="diff_deletion">',
            '~': '<div class="diff_addition"><div class="diff_deletion">',
            '!': '<div class="diff_deletion"><div class="diff_addition">',
            '|': ''
        };

        var ends = {
            '+': '</div>',       '-': '</div>',
            '~': '</div></div>', '!': '</div></div>',
            '|': ''
        };

        var processed_lines = [];
        var last_outputted_line = -1;
        var colorized;

        var i;
        for ( i = 0; i < diff.length; i++ )
        {
            var d = diff[i];
            
            // output the context before the diff
            var context_begin = Math.max(last_outputted_line + 1, d.beg - contextSize);

            // write an elipssiss if there is a deconnection
            if (context_begin > last_outputted_line + 1 && i != 0)
                processed_lines.push("...");

            highlighter.set_current_line_number(context_begin);
            for ( var lineNum = context_begin; lineNum < d.beg; lineNum++ )
            {
                processed_lines.push(highlighter.colorLine(lines[lineNum]));
            }

            // output the real diff
            if (d.end - d.beg <= 1)
            {
                highlighter.set_current_line_number(d.beg);
                colorized = highlighter.colorLine(lines[d.beg]);
                processed_lines.push(begs[d.way] + colorized  + ends[d.way]);
            }
            else
            {
                highlighter.set_current_line_number(d.beg);
                colorized = highlighter.colorLine(lines[d.beg]);
                processed_lines.push( begs[d.way] + colorized);

                for ( var lineNum = d.beg + 1; lineNum < d.end - 1; lineNum++ )
                    processed_lines.push(highlighter.colorLine(lines[lineNum]));

                processed_lines.push(highlighter.colorLine(lines[d.end - 1]) + ends[d.way]);
            }

            var next_commit_begin = (i === diff.length - 1) ? lines.length - 1 : diff[i + 1].beg;
            var context_end = Math.min(d.end + contextSize, next_commit_begin - 1);
            highlighter.set_current_line_number(d.end + 1);
            for ( var lineNum = d.end + 1; lineNum <= context_end; lineNum++ ) {
                processed_lines.push(highlighter.colorLine(lines[lineNum]));
            }

            last_outputted_line = context_end;
        }

        return processed_lines.join("\n");
    }

    /** Combine two diff list into one list of edition
     * ranges.
     * @param removings [{way: String, orig_idx: Int, size: Int}]
     * @param addings   [{way: String, dest_idx: Int, size: Int}]
     * @return [{way: String, beg: Int, end: Int}]
     */
    var calculate_fold_set = function (removings, addings) {
        var ranges = [];
        var lefts = _.toArray(_.map(removings, function (c) { 
            return { way: '-', beg: c.orig_idx, end: c.orig_idx + c.size}
        }));

        var rights = _.toArray( _.map(addings, function (c) { 
                return { way: '+', beg: c.dest_idx, end: c.dest_idx + c.size}
        }));

        var combiner = function(a,b) {
            if (a == '+' && b == '-')
                { return '~' }
            else
                { return '!' }
        };

        var left_read_index = 0;
        var right_read_index = 0;

        var left = lefts[left_read_index];
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
            // ############
            //                  ############
            else if (left.beg < right.beg && left.end < right.beg)
            {
                ranges.push({ way: left.way, beg: left.beg, end: left.end });
                inc_left();
            }
            else if (left.beg < right.beg && left.end <= right.end)
            {
                ranges.push({ way: left.way, beg: left.beg, end: right.beg - 1 });
                ranges.push({ way: combiner(left.way, right.way)
                        , beg: right.beg, end: left.end });
                right.beg = left.end + 1;
                inc_left();
            }
            else if (left.beg == right.beg)
            {
                if (left.end <= right.end )
                {
                    ranges.push({ way: combiner(left.way, right.way)
                            , beg: left.beg, end: left.end });
                    right.end = left.end + 1;
                    inc_left();
                }
                else
                {
                    ranges.push({ way: combiner(left.way, right.way)
                            , beg: left.beg, end: right.end });
                    left.beg = right.end + 1;
                    inc_right();
                }
            }
            else if (left.beg < right.beg && left.end > right.end)
            {
                ranges.push( {way: left.way, beg: left.beg, end: right.beg - 1  } );
                ranges.push({ way: combiner(left.way, right.way)
                            , beg: right.beg, end: right.end });
                left.beg = right.end + 1;
                inc_right();
            }
        }

        while (left_read_index < lefts.length)
            { ranges.push(lefts[left_read_index++]); }

        while (right_read_index < rights.length)
            { ranges.push(rights[right_read_index++]);  }

        return ranges;
    }

    var intercalate_diff_add = function (lines, diff_list) {
        for ( var idx in diff_list )
        {
            var diff = diff_list[idx];
            if (diff.way === "+")
            {
                lines[diff.dest_idx] = '<div class="diff_addition">' + lines[diff.dest_idx];
                lines[diff.dest_idx + diff.size - 1] += '</div>';
            }
            
        }
        return lines;
    }

    var intercalate_diff_del = function (lines, diff_list) {
        for ( var idx in diff_list )
        {
            var diff = diff_list[idx];
            if (diff.way === "-")
            {
                lines[diff.orig_idx] = '<div class="diff_deletion">' + lines[diff.orig_idx];
                lines[diff.orig_idx + diff.size - 1] += '</div>';
            }
            
        }
        return lines;
    }

    /** Keep only the diff information of adding (with a '+' way property)
     * @param diffs [{way: String, ...}]
     */
    var filter_adds = function(diffs) {
        return _.filter(diffs, function (c) { return c.way === '+'; })
    }

    /** Keep only the diff information of adding (with a '-' way property)
     * @param diffs [{way: String, ...}]
     */
    var filter_rems = function(diffs) {
        return _.filter(diffs, function (c) { return c.way === '-'; })
    }

    return {
        generateCompactHtml: generate_compact_html,
        intercalateDiffDel:  intercalate_diff_del,
        intercalateDiffAdd:  intercalate_diff_add,
        filterAdds: filter_adds,
        filterRems: filter_rems,
        calculateFoldSet: calculate_fold_set 
    };
})();

var html_encodize = function(snipp) {
    return snipp.replace(/\&/g, '\&amp;').replace(/</g, '\&lt;').replace(/</g, '\&gt;');
}
////////////////////////////////////////////////////////////
////  Commit
////////////////////////////////////////////////////////////
/**
 * @constructor
 */
var Commit = function(key, data) {
    this.key = key;
    this.parents_sha = data.parents_sha;
    this.file_changes = data.file_changes;
    this.message = data.message;
    this.split_message = (html_encodize(data.message)).replace(/\n/g, '<br/>');

    var kind_formater = {
        'modification': function(e) {
            var hl = TinySyntaxHighlighter.from_filename(true, e.name);
            var rez_node = ich.commit_file_modification_detailed(e);
            var code_node = rez_node.find('pre');

            var curr_diff;
            var acc = ""
            for ( var i = 0; i < e.diff.length; i++ )
            {
                curr_diff = e.diff[i];
                if (curr_diff.way == '+') {
                    acc += '<div class="diff_addition">';
                    hl.set_current_line_number(curr_diff.dest_idx);
                }
                else {
                    acc +=  '<div class="diff_deletion">';
                    hl.set_current_line_number(curr_diff.orig_idx);
                }

                for ( var l = 0; l < curr_diff.data.length; l++ )
                    acc += hl.colorLine(curr_diff.data[l] + "\n")

                if (i < e.diff.length - 1)
                    acc += "</div>\n...\n";
            }

            code_node.html(acc);

            return rez_node;
        },
        'addition':ich.commit_file_addition,
        'deletion':ich.commit_file_deletion
    };

    this.create_dom = function() {
        this.orig_node = ich.commit_detailed(this);
        $('.container').prepend(this.orig_node);
        return this.orig_node;
    }

    this.render = function() {
        for ( var change in this.file_changes ) {
            var e = this.file_changes[change];
            var kind = e['kind'];
            e.key = this.key;

            var file_diff;
            if (kind_formater.hasOwnProperty(kind))
                this.orig_node.append(kind_formater[kind](e));
            else
                this.orig_node.append(ich.commit_file_unknown(e));
        }
    }
    
    return this;
};

/**
 * @constructor
 */
var CommitRenderer = (function() {
    var fetch_commit = function( id, f ) {
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

    var init = function(init_data) {
        var new_commit = new Commit(init_data.key, init_data);
        new_commit.create_dom();
        new_commit.render();
        
        this.collection = [new_commit];
        this.keys[init_data.key] = new_commit;
    }

    var init_methods = function() {
        this.keys = {};
        this.render_all = function() {
            for ( var i = 0; i < this.collection.length; i++ ) {
                this.collection[i].render();
            }
        };


        this.create_all_dom = function() {
            for ( var i = 0; i < this.collection.length; i++ ) {
                this.collection[i].create_dom();
            }
        };

        this.fetch_previous = function() {
            var this_obj = this;
            var prev_id = this.collection[this.collection.length - 1].parents_sha[0];

            fetch_commit(prev_id, function(data) {
                var new_commit = new Commit(data.key, data);

                var new_node = new_commit.create_dom();
                new_node.animate({'width':'toggle'}, 0);
                new_commit.render();
                new_node.animate({'width':'toggle'}, application_state.apparition_duration());

                this_obj.collection.push(new_commit);
                this_obj.keys[data.key] = new_commit;
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
                init.call(rez, data);
            });

            return rez;
        }
    };
})();

/**
 * @constructor
 */
var FileBlob = function (filename, data) {
    var add_syntax_coloration = function( filename, lines )
    {
        var ret = [];
        var highlighter = TinySyntaxHighlighter.from_filename(true, filename);

        for ( var i = 0; i < lines.length; i++ )
            ret.push(highlighter.colorLine(lines[i]));

        return ret;
    }

    this.detail_fetched = false;
    this.file = filename;
    this.diff = data.diff;
    this.data = data.data;
    this.filekey = data.filekey;
    this.key = data.key;
    this.message = data.message;
    this.parent_commit = data.parent_commit;
    this.path = data.path;

    this.create_dom_details = function() {
        var kind_formater = {
            'modification': ich.commit_file_modification,
            'addition':ich.commit_file_addition,
            'deletion':ich.commit_file_deletion
        };

        var detail_node = $('#' + this.key + ' .commit_detail');
        detail_node.append(ich.commit_button_file({commit: this.key}));

        for ( var i = 0; i < this.details.length; i++ )
        {
            var e = this.details[i];
            var kind = e['kind'];
            e.key = this.key;

            var file_diff;
            if (kind_formater.hasOwnProperty(kind))
                detail_node.append(kind_formater[kind](e));
            else
                detail_node.append(ich.commit_file_unknown(e));
        }
    };

    this.toggle_detail_pane = function() {
        var detail = $('#' + this.key + ' .commit_detail');
        var btn_node = $('#' + this.key + ' .more_info');
        var btn_text = btn_node.text()

        if (btn_text[0] == '▼')
            btn_node.html("&#x25b2");
        else
            btn_node.html("&#x25bc");

        detail.animate({height: 'toggle'}, application_state.apparition_pane_duration);
    };

    this.fetch_details = function() {
        if (this.detail_fetched)
        {
            this.toggle_detail_pane();
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
            success: function(data) {
                if (data === null) {
                    show_error({error: 'Communication error with the server'});
                    return;
                }

                this_obj.detail_fetched = true;

                if (data['error']) { 
                    show_error( data );
                    return;
                }

                var kind_formater = {
                    'modification': ich.commit_file_modification,
                    'addition':ich.commit_file_addition,
                    'deletion':ich.commit_file_deletion
                };

                this_obj.details = data;

                var detail_node = $('#' + this_obj.key + ' .commit_detail');
                detail_node.animate({height: 'toggle'}, 0);
                this_obj.create_dom_details();
                this_obj.toggle_detail_pane()
            }
        });
    };

    this.render_file_data = function(prev_diff) {
        var filename = this.file;
        var diff     = this.diff;
        var data     = this.data;

        var rems = DiffManipulator.filterRems(diff);
        var adds = DiffManipulator.filterAdds(prev_diff);
        var ranges = DiffManipulator.calculateFoldSet(rems, adds);

        var clean_cr_lf_data = data.replace(/\r/g, '');

        if (application_state.active_view_mode() === 'compact')
            return DiffManipulator.generateCompactHtml(filename,
                                                    application_state.active_context_size(),
                                                    true, clean_cr_lf_data, ranges);
        else // render full
        {
            var lines = add_syntax_coloration(filename, clean_cr_lf_data.split('\n'));
            var encoded_with_diff = DiffManipulator.intercalateDiffDel(lines, rems);
            var diffed_content = DiffManipulator.intercalateDiffAdd(encoded_with_diff, adds);
            return diffed_content.join('\n');
        }
    }

    this.create_dom = function() {
        this.short_message = this.message.split("\n")[0];
        var processed;

        var path_length = this.path.length;
        var maximum_path_length = 15;

        if (path_length > maximum_path_length)
        {
            this.path_beg = _.first(this.path, maximum_path_length / 2);
            this.path_end = _.last(this.path, maximum_path_length / 2);
            processed = ich.commitfile_huge_path(this);
        }
        else
            processed = ich.commitfile(this);

        $(".container").prepend( processed );
                
        if (this.detail_fetched)
        {
            this.create_dom_details();
            this.toggle_detail_pane();
            $('#' + this.key + ' .more_info').html("&#x25bc");
        }

        return processed;
    }

    this.render = function(prev_diff) {
        var rendered = this.render_file_data(prev_diff);
        $("#" + this.key + " .syntax_highlighted").html(rendered);
    }

    return this;
};

/**
 * @constructor
 */
var FileRenderer = (function() {

    var fetch_file = function(file, commit, filekey, f) {
        var params = { commit: commit, last_file: filekey };
        var request = '/ask_parent';

        if (file[0] == '/') request += file;
        else request += '/' + file;

        $.ajax({ url: request, dataType: 'json', data: params,
                 error: function() {
                     show_error({error: 'Communication error with the server while fetching file'});
                 },
                 success: f });
    };

    var init = function(init_data) {
        var init_file = new FileBlob(init_data.file, init_data);

        this.collection = [init_file];
        this.keys[init_file.key] = init_file;

        init_file.create_dom();
        init_file.render([]);

        return this;
    }

    var init_methods = function(init_data) {

        this.collection = [];
        this.keys = {};

        this.create_all_dom = function() {
            for ( var i = this.collection.length - 1; i >= 0; i-- ) {
                this.collection[i].create_dom();
            }
        };

        this.render_all = function() {
            var i;
            var prev_diff = [];

            for ( i = 0; i < this.collection.length; i++ ) {
                this.collection[i].render(prev_diff);
                prev_diff = this.collection[i].diff;
            }
        };

        this.fetch_details = function(commit_id) {
            this.keys[commit_id].fetch_details();
        };

        this.send_message = function( msg ) {
            if (msg.action === 'fetch_detail')
                return this.fetch_details(msg.key);
        };

        this.fetch_previous = function() {
            var last_commit = this.collection[0];
            var this_obj = this;

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

                var new_commit = new FileBlob(last_commit.file, data);

                var node = new_commit.create_dom();

                this_obj.collection.unshift( new_commit );

                this_obj.keys[new_commit.key] = new_commit;
                node.animate({'width': 'toggle'}, 0);
                this_obj.collection[0].render([]);
                this_obj.collection[1].render(new_commit.diff);
                node.animate({'width': 'toggle'}, application_state.apparition_duration() * 2);
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

function show_error( data )
{
    $('.message_carret').html( data.error )
}


function leave_server()
    { $.ajax( {url:"/quit", async:false} ); }

ich.grabTemplates();
