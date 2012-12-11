var FileComparer = (function() {
    var init_methods = function() {

        this.fetch_previous = function(id) {
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
            var created = ich.compare_files(this);
            $('.container').append(created);


            var padders = $('.container .file_content .align_padder pre');
            
            var this_obj = this;
            var number_columns = $('.line_number_column', created);

            var translaters = [
                function (line) { return DiffManipulator.toNextDiff(line, this_obj.data.diff); },
                function (line) { return DiffManipulator.toPrevDiff(line, this_obj.data.diff); }
            ];

            for (var i = 0; i < number_columns.length; i++) {
                (function(n) {
                    var side_id = n;
                    var other_id = 1 - n;

                    $(number_columns[i]).click(function(event) {
                        var line = parseInt(event.originalEvent.target.textContent) - 1;
                        var next_line = (translaters[side_id])(line);
                        var line_diff = Math.abs(line - next_line);

                        var string_padder = '';

                        if (line_diff > 0) {
                            string_padder = '\n ';
                            for (var i = 1; i < line_diff; i++) {
                                string_padder += '\n ';
                            }
                        }

                        if (next_line >= line) {
                            $(padders[side_id]).text(string_padder);
                            $(padders[other_id]).text('');
                        } else {
                            $(padders[side_id]).text('');
                            $(padders[other_id]).text(string_padder);
                        }
                    });
                })( i );
            }
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


