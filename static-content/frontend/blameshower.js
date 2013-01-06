/** @constructor */
var BlameShower = function(data) {
    this.data = data;

    var ranges = this.data.ranges;

    for (var i = 0; i < ranges.length; i++) {
        ranges[i].padd_string = this.returnGenerator(ranges[i]);
    }

    this.create_all_dom();
    this.render_all();
}

BlameShower.prototype.returnGenerator = function(range) {
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

BlameShower.prototype.set_backgrounds_colors = function() {
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

BlameShower.prototype.fetch_previous = function(id) {
    show_error({error: 'Does not exists in this mode'});
};

BlameShower.prototype.render_all = function() {
    var render_node = $('.syntax_highlighted', this.orig_node);
    var number_node = $('.line_number_column', this.orig_node);
    var clean_cr_lf_data = this.data.data.replace(/\r/g, '');

    DiffManipulator.generateFullHtml(this.data.filename, false,
                                        clean_cr_lf_data, [],
                                        number_node[0], render_node[0]);
    this.set_backgrounds_colors();
};

BlameShower.prototype.create_all_dom = function() {
    this.orig_node = ich.blamefile(this.data);
    $('.container').append(this.orig_node);
};

BlameShower.prototype.send_message = function( msg ) {
};

BlameShower.prototype.gui_descr =
    { compact_view: false, fetch_previous: false
    , context_size: false, syntax_toggle: false };

BlameShower.create_from_data = function(data) {
    return new BlameShower(data);
};

BlameShower.create_from_arg = function(key, file, f) {
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

            f(new BlameShower(data));
        }
    });
};

