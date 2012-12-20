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
                this.fetch_previous(0);
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

        this.command_request = function() {
            var command = $('.command_line_file');
            var input = $('input', command);
            var form = $('form', command);

            command.css("visibility", "visible");

            var this_obj = this;
            form.submit(function () {
                var val = $(input).val();

                if (val[0] === ':')
                    val = val.slice(1, val.length);


                input.blur();
                command.css("visibility", "hidden");
                form.unbind('submit');
                    
                return false;
            });

            input.focus();
            input.val('');
            return false;
        }

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
            else if (msg.action === Project.GuiMessage.COMMAND_REQUEST)
                return this.command_request();
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

        this.fetch_previous = function(id) {
            var this_obj = this;
            var parents = this.collection[this.collection.length - 1].parents_sha;
            var prev_id = parents.length > 0 ? parents[0].key : null_ref;

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


