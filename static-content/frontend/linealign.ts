/// <reference path="jquery.d.ts" />

class FileAlign {
	public create_padding(line_diff : number) : string {
		var string_padder = '';
	
		if (line_diff > 0) {
			string_padder = '\n ';
			for (var i = 1; i < line_diff; i++) {
				string_padder += '\n ';
			}
		}
	
		return string_padder;
	}
	
	public make_number_clickable(node : Node | JQuery,
	                             callback: (s: number, line: number) => void) {
		var number_columns = $('.line_number_column', node);
		for (var i = 0; i < number_columns.length; i++) {
			(function(n : number) {
				var side_id = n;
				$(number_columns[i]).click(function(event) {
					var line = parseInt((<HTMLElement>event.originalEvent.target).textContent, 10) - 1;
					callback(side_id, line);
				});
			})( i );
		}
	}
    
    
    public clear_command() {
        var command = $('.command_line');
        var input = $('input', command);
        var form = $('form', command);
    
        input.blur();
        command.css("visibility", "hidden");
        form.unbind('submit');
        input.unbind('keyup');
    }
    
    public command_request(absolute_callback : (line: number) => void,
                           relative_callback : (offset: number) => void) {
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
    }
    
    public static move_line_up(line_index : number, node : Node | JQuery) {
        if (line_index === 0)
            return line_index;
    
        var numbers = $('.syntax_line_number', node);
        $(numbers[line_index]).removeClass('highlighted_line');
        line_index = line_index - 1;
        $(numbers[line_index]).addClass('highlighted_line');
        return line_index;
    }
    
    public static move_line_down(line_index : number, node : Node | JQuery) {
        var numbers = $('.syntax_line_number', node);
        if (line_index === numbers.length)
            return line_index;
    
        $(numbers[line_index]).removeClass('highlighted_line');
        line_index = line_index + 1;
        $(numbers[line_index]).addClass('highlighted_line');
    
        return line_index;
    }
}
