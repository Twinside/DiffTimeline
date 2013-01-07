/** @constructor */
var BranchLister = function() {
    this.is_opened = false;
    this.fetch_branch_list();
};

BranchLister.prototype.fetch_branch_list = function () {
    var this_obj = this;

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

            this_obj.data = data;
            var new_node = ich.branch_list({ref_list: data});

            $('body').append(new_node);
            make_draggable_elems(new_node);
            this_obj.node = new_node;
            this_obj.setup_branch_toggle();
            this_obj.install_search_hook();
        }
    });
};

BranchLister.prototype.install_search_hook = function () {
    var input = $('input', this.node);
    var this_obj = this;
    var branch_view = $('.global_branch', this_obj.node);

    input.keyup(function() {
        var pattern = new RegExp($(input).val(), 'i');
        var counter = 0;

        for ( var j = 0; j < this_obj.data.length; j++) {
            var group = this_obj.data[j];

            for ( var i = 0; i < group.branches.length; i++ )
            {
                var el = group.branches[i];

                if (pattern.test(el.name)) {
                    $(branch_view[counter]).show();
                } else {
                    $(branch_view[counter]).hide();
                }

                counter++;
            }
        }
    });
};

BranchLister.prototype.setup_branch_toggle = function() {  
    $('.branch_list .list').animate({width: 'toggle'}, 0);

    var this_obj = this;

    $('.branch_list .toggler').click( function( event ){
        $('.branch_list .list').animate({width: 'toggle'}, 100);
        if (this_obj.is_opened) {
            $('.branch_list .toggler div').html('&#x25bc; Branches');
        } else {
            $('.branch_list .toggler div').html('&#x25b2 Branches');
        }

        this_obj.is_opened = !this_obj.is_opened;
    })
};


