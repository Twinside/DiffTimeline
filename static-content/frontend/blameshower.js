var BlameShower = (function() {
    "use strict";

    var returnGenerator = function(range) {
        var n = range.size - 1;
        var ret = '';
        var messageLines = range.tag.message.split('\n');
        var i = 0;

        var splitedMessageLines = [];
        for (i = 0; i < messageLines.length; i++) {
            var sub = messageLines[i].match(/.{1,30}/g);

            if (!sub) continue;
            if (sub.length > 1) {
                for (var j = 0; j < sub.length; j++) {
                    splitedMessageLines.push(sub[j] + " ...");
                }
            }
            else splitedMessageLines.push(sub[0]);
        }

        if (n > 0) {
            ret += '\n(' + range.tag.author + ')';
        }

        for (i = 1; i - 1 < splitedMessageLines.length && i < n; i++) {
            ret += '\n' + splitedMessageLines[i - 1];
        }

        for (i = i; i < n; i++) ret += '\n&nbsp;';
        return ret;
    }

    var init = function(data) {
        this.data = data;

        var ranges = this.data.ranges;

        for (var i = 0; i < ranges.length; i++) {
            ranges[i].padd_string = returnGenerator(ranges[i]);
        }

        this.create_all_dom();

        this.render_all();
    };

    var fetch = function( obj, key, file ) {
        var this_obj = obj;
        if (file[0] != '/') file = '/' + file;

        $.ajax({ url: '/blame/' + encodeURIComponent(key) + file,
            dataType: 'json',
            data: {},
            error: function() {
                show_error({error: 'Communication error with the server while fetching blame'});
            },
            success: function(data) {
                if (data['error']) { 
                    show_error( data );
                    return;
                }

                init.call(this_obj, data);
            }
        });
    }

    var init_methods = function() {
        this.set_backgrounds_colors = function() {
            var ranges = this.data.ranges;

            var nodes = $('.blame_range', this.orig_node);
            var date_interval = this.data.latest - this.data.earliest;

            for (var i = 0; i < ranges.length; i++) {
                var range = ranges[i];
                var zeroToOne = (range.tag.timestamp - this.data.earliest) / date_interval;
                var newColor = color_lerp(blame_gradient.beg, blame_gradient.end, zeroToOne);

                $(nodes[i]).css('background-color', newColor);
                $(nodes[i]).hover(function() {
                    $(this).addClass('blame_hover');
                }, function() {
                    $(this).removeClass('blame_hover');
                });
            }
        }

        this.fetch_previous = function(id) {
            show_error({error: 'Does not exists in this mode'});
        };

        this.render_all = function() {
            var render_node = $('.syntax_highlighted', this.orig_node);
            var number_node = $('.line_number_column', this.orig_node);
            var clean_cr_lf_data = this.data.data.replace(/\r/g, '');

            DiffManipulator.generateFullHtml(this.data.filename, false,
                                             clean_cr_lf_data, [],
                                             number_node[0], render_node[0]);
            this.set_backgrounds_colors();
        };

        this.create_all_dom = function() {
            this.orig_node = ich.blamefile(this.data);
            $('.container').append(this.orig_node);
        };

        this.send_message = function( msg ) {
        };

        this.gui_descr = { compact_view: false, fetch_previous: false
                         , context_size: false, syntax_toggle: false };
    }

    return {
        create_from_data: function(data) {
            var created = new init_methods();
            init.call(created, data);
            return created;
        },

        create_from_arg: function(key, file) {
            var created = new init_methods();
            fetch( created, key, file );
            return created;
        }
    };
})();


