var CommitComparer = (function() {
    var init_methods = function() {

        this.fetch_previous = function(id) {
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

