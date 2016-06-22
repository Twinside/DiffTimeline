/// <reference path="jquery.d.ts" />
/// <reference path="resultset.ts" />
/// <reference path="commit.ts" />

class CommitComparer implements ResultSet {
    private branch_a : ref;
    private branch_b : ref;
    private last_comparison : Commit;
    
    public constructor(b1 : ref, b2 : ref) {
        this.branch_a = b1;
        this.branch_b = b2;
        this.refresh_diff();
    }
    
    public fetch_previous(id : any) {
        show_error({error: 'Does not exists in this mode'});
    };
    
    public render_all() {
        /* nothing */
        if (this.last_comparison)
            this.last_comparison.render();
    }
    
    private refresh_diff() {
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
    
    public create_all_dom() {
        $('.container').append(this.last_comparison.create_dom());
    }
    
    public send_message(msg : {action: string}) {
        if (this.last_comparison)
            this.last_comparison.send_message(msg);    
    }
            
    public static create_from_args(b1 :ref, b2 : ref) {
        return new CommitComparer(b1, b2);
    }

    public gui_descr =
        { compact_view: false, fetch_previous: false
        , context_size: false, syntax_toggle: false };
}
