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
    $(document).scrollTo( { top: 0, left: 0 } );
});

