/**
 * @constructor
 * @implements {ResultSet}
 */
var CommitComparer = function(b1, b2) {
    this.branch_a = b1;
    this.branch_b = b2;
    this.refresh_diff();
}

CommitComparer.prototype.fetch_previous = function(id) {
    show_error({error: 'Does not exists in this mode'});
};

CommitComparer.prototype.render_all = function() {
    /* nothing */
    if (this.last_comparison)
        this.last_comparison.render();
};

CommitComparer.prototype.refresh_diff = function() {
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

CommitComparer.prototype.create_all_dom = function() {
    $('.container').append(this.last_comparison.create_dom());
};

CommitComparer.prototype.send_message = function( msg ) {
    if (this.last_comparison)
        this.last_comparison.send_message(msg);
};

CommitComparer.prototype.gui_descr =
    { compact_view: false, fetch_previous: false
    , context_size: false, syntax_toggle: false };

CommitComparer.create_from_args = function(b1, b2) {
    return new CommitComparer(b1, b2);
};

