/** @constructor */
var FileAlign = function() {
}

FileAlign.prototype.create_padding = function(line_diff) {
    var string_padder = '';

    if (line_diff > 0) {
        string_padder = '\n ';
        for (var i = 1; i < line_diff; i++) {
            string_padder += '\n ';
        }
    }

    return string_padder;
};

FileAlign.prototype.make_number_clickable = function(node, callback) {
    var number_columns = $('.line_number_column', node);

    for (var i = 0; i < number_columns.length; i++) {
        (function(n) {
            var side_id = n;

            $(number_columns[i]).click(function(event) {
                var line = parseInt(event.originalEvent.target.textContent, 10) - 1;
                callback(side_id, line);
            });
        })( i );
    }
};

FileAlign.prototype.clear_command = function() {
    var command = $('.command_line');
    var input = $('input', command);
    var form = $('form', command);

    input.blur();
    command.css("visibility", "hidden");
    form.unbind('submit');
    input.unbind('keyup');
}

FileAlign.prototype.command_request = function(absolute_callback, relative_callback) {
    var command = $('.command_line');
    var input = $('input', command);
    var form = $('form', command);

    command.css("visibility", "visible");

    var this_obj = this;
    var is_first = true;
    input.keyup(function (e) {
        if (is_first) {
            $(input).val('');
            is_first = false;
            return;
        }

        // detect the escape key
        if (e.keyCode == 27) {
            this_obj.clear_command();
            return true;
        }

        return false;
    });

    form.submit(function () {
        var val = $(input).val();

        if (val[0] === ':')
            val = val.slice(1, val.length);

        // line number jumping
        if ( val.match(/^[0-9]+$/) ) {
            var line = parseInt(val, 10) - 1;
            absolute_callback(line);
        }
        else if ( val.match(/^[+-][0-9]+$/) ) {
            var offset = parseInt(val, 10);
            relative_callback(offset);
        }

        this_obj.clear_command();
        return false;
    });

    input.focus();
    return false;
};

FileAlign.move_line_up = function(line_index, node) {
    if (line_index === 0)
        return line_index;

    var numbers = $('.syntax_line_number', node);
    $(numbers[line_index]).removeClass('highlighted_line');
    line_index = line_index - 1;
    $(numbers[line_index]).addClass('highlighted_line');
    return line_index;
}

FileAlign.move_line_down = function(line_index, node) {
    var numbers = $('.syntax_line_number', node);
    if (line_index === numbers.length)
        return line_index;

    $(numbers[line_index]).removeClass('highlighted_line');
    line_index = line_index + 1;
    $(numbers[line_index]).addClass('highlighted_line');

    return line_index;
};

