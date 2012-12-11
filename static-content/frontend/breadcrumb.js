/**
 * @namespace
 */
var breadcrumb = (function() {
    "use strict";

    /** @type {number} */
    var count = 0;

    /** @type {number} */
    var current_index = 0;

    return {
        append_breadcrumb:
		/** @type {function(string) : void} */
						   function( name ) {
            if (current_index < count - 1)
            {
                $('#breadcrumb > span').slice(current_index + 1).remove();
                count = current_index + 1;
            }

            /** @type {jQuery} */
            var elem = ich.breadcrumbelem({name:name, id:count});
            $('#breadcrumb').append(elem);
            current_index = count++;
        },

        go_forward: function() {
            if (current_index === count - 1)
                return;

            current_index++;
            Project.state.jump_context(current_index);
        },

        go_backward: function() {
            if (current_index === 0)
                return;

            current_index--;
            Project.state.jump_context(current_index);
        },

        click_index: 
        /** @type {function(number) : void} */
					function( idx ) {
            current_index = idx;
            Project.state.jump_context(idx);
        }
    };
})();

