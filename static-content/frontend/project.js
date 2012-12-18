var Project = {};

/** @enum {number} */
Project.ViewMode = {
    VIEW_FULL: 1,
    VIEW_COMPACT: 0
};

/**
 * @const
 * @enum {string}
 */
Project.DiffChar = {
    DIFF_ADD: '+',
    DIFF_DEL: '-',
    DIFF_DELADD: '~',
    DIFF_ADDDEL: '!'
};

/**
 * @const
 * @enum {string}
 */
Project.DiffKind = {
    KIND_MODIFICATION: 'modification',
    KIND_ADDITION: 'addition',
    KIND_DELETION: 'deletion'
}

/**
 * @const
 * @enum {number}
 */
Project.GuiMessage = {
    FETCH_TREE:   0,
    FETCH_DETAIL: 1,
    MOVE_LEFT:    2,
    MOVE_RIGHT:   3,
    MOVE_UP:      4,
    MOVE_DOWN:    5,
    MOVE_FIRST:   6,
    MOVE_LAST:    7,

    MOVE_INNER:      8,
    COMMAND_REQUEST: 9
}

/**
 * @type {Object}
 */
Project.state = (function () {
    "use strict";

    /** @type {Project.ViewMode} */
    var view_mode = Project.ViewMode.VIEW_FULL;

    /** @type {boolean} */
    var apply_syntax_coloration = true;

    /** @type {number} */
    var context_size = 2;

    /** @type {number} */
    var max_commit_delta_show = 15;

    /** @type {Array.<ResultSet>} */
    var states = [];

    /** @type {Array.<ResultSet>} */
    var forward_states = [];

    /** @type {number} */
    var commit_delta_margin = 6;

    /** @type {number} */
    var apparition_duration = 750;

    /** @type {number} */
    var apparition_pane_duration = 500;

    /** @type {Object.<Project.ViewMode, string>} */
    var btn_toggle_text = {}
    btn_toggle_text[Project.ViewMode.VIEW_FULL] =  '&#x25bc;<br/>&#x25b2;';
    btn_toggle_text[Project.ViewMode.VIEW_COMPACT] =  '&#x25b2;<br/>&#x25bc;';

    var show_hide_toolbar_elements = function(descr) {
        if (descr.compact_view)
            $('.btn_toggleview').show();
        else
            $('.btn_toggleview').hide();

        if (descr.fetch_previous)
            $('.btn_returnpast').show();
        else
            $('.btn_returnpast').hide();

        if (descr.context_size)
            $('.context_size').show();
        else
            $('.context_size').hide();

        if (descr.syntax_toggle)
            $('.syntax_highlight_toggle').show();
        else
            $('.syntax_highlight_toggle').hide();

    };

    return {
        /** @type {function() : Project.ViewMode} */
        active_view_mode: function() { return view_mode; },

        /** @type {function() : number} */
        active_context_size: function() { return context_size; },

        /** @type {function() : number} */
        apparition_duration: function() { return apparition_duration; },

        /** @type {function(string) : void} */
        send_state_message: function(message) {
            var last_path = states[ states.length - 1 ];
            last_path.send_message(message);
        },

        /** @type {function() : void} */
        create_all_dom: function() {
            var last_path = states[ states.length - 1 ];
            show_hide_toolbar_elements(last_path.gui_descr);
            last_path.create_all_dom();
        },

        render_all: function() {
            var last_path = states[ states.length - 1 ];
            last_path.render_all();
        },

        set_previous_button_count: function( count, tooltips ) {
            var i;
            var container = $('div .return_past_container');
            container.children().remove();

            for (i = 0; i < count; i++) {
                var tooltip = i < tooltips.length ? tooltips[i] : "";
                var sub_node = ich.fetch_previous(
                    {id: i, tooltip: tooltip}
                );
                $(container).append( sub_node[0] );
            }
        },

        get_previous: function(id) {
            var last_path = states[ states.length - 1 ];
            last_path.fetch_previous(id);
        },

        /**
         * @type {function(ref, string) : void}
         */
        switch_blame: function(start_commit, file) {
            this.clear_display();
            var new_state = BlameShower.create_from_arg(start_commit, file);
            states.push( new_state );
            show_hide_toolbar_elements(new_state.gui_descr);
            breadcrumb.append_breadcrumb("Blame (" + file + ")");
        },

        /**
         * @type {function(string, ref, ref) : void}
         */
        switch_file: function(file, fkey, start_commit) {
            this.clear_display();
            var new_state = FileRenderer.create_from_arg(file, fkey, start_commit);
            states.push( new_state );
            show_hide_toolbar_elements(new_state.gui_descr);
            breadcrumb.append_breadcrumb(file);
        },

        switch_commit_comp: function( b1, b2 ) {
            this.clear_display();
            var new_state = CommitComparer.create_from_args(b1, b2);
            states.push( new_state );
            show_hide_toolbar_elements(new_state.gui_descr);
            breadcrumb.append_breadcrumb('Compare commit');
        },

        switch_file_comp: function(key1, file1, key2, file2) {
            this.clear_display();
            var new_state = FileComparer.create_from_args(key1, file1, key2, file2);
            states.push( new_state );
            show_hide_toolbar_elements(new_state.gui_descr);
            breadcrumb.append_breadcrumb('Compare file');
        },

        /** @type {function(ref) : void} */
        switch_commit: function(id) {
            this.clear_display();
            var new_state = new CommitRenderer.create_from_arg(id);
            states.push(new_state);

            if (id !== null_ref)
                breadcrumb.append_breadcrumb(id);
            else
                breadcrumb.append_breadcrumb('HEAD');

            show_hide_toolbar_elements(new_state.gui_descr);
        },

        /** @type {function(number) : void} */
        jump_context: function(idx) {
            if (states.length - 1 == idx) return;

            var i;

            if (idx < states.length - 1) {
                while (idx != states.length - 1)
                    forward_states.push(states.pop());
            } else {
                while (idx != states.length - 1)
                    states.push(forward_states.pop());
            }

            this.clear_display();
            this.create_all_dom();
            this.render_all();
            /* TODO restore button count */
        },

        clear_display: function () {
            this.set_previous_button_count(1, []);
            $('.container > *').remove();
        },

        increase_context_size: function() {
            context_size = context_size + 1;
            $('.toolbar div textarea').text(context_size.toString());

            if (view_mode === Project.ViewMode.VIEW_COMPACT) { this.render_all(); }
        },

        decrease_context_size: function() {
            context_size = Math.max(0, context_size - 1);
            $('.toolbar div textarea').text(context_size.toString());

            if (view_mode === Project.ViewMode.VIEW_COMPACT) { this.render_all(); }
        },

        toggle_diff_full: function() {
            var ranges = null;

            if (view_mode === Project.ViewMode.VIEW_FULL)
                view_mode = Project.ViewMode.VIEW_COMPACT;
            else view_mode = Project.ViewMode.VIEW_FULL;

            $('.btn_toggleview').html(btn_toggle_text[view_mode]);
            this.render_all();
        },

        chrome_scroll_offset: function() {
            return {top:-120, left:-120};
        },

        start_blame: function(blame_obj) {
            var new_state = BlameShower.create_from_data(blame_obj);
            states.push( new_state );
            breadcrumb.append_breadcrumb('Blame (' + blame_obj.filename + ')');
            show_hide_toolbar_elements(new_state.gui_descr);
        },

        start_commit: function(commit_obj) {
            var new_state = CommitRenderer.create_from_data(commit_obj);
            states.push( new_state );
            show_hide_toolbar_elements(new_state.gui_descr);

            if (commit_obj.key !== null_ref)
                breadcrumb.append_breadcrumb(commit_obj.key);
            else
                breadcrumb.append_breadcrumb('HEAD');
        },

        start_file: function(file_obj) {
            var new_state = FileRenderer.create_from_data(file_obj);
            states.push( new_state );
            show_hide_toolbar_elements(new_state.gui_descr);
            breadcrumb.append_breadcrumb(file_obj.filename);
        },

        check_comparison: function(node_a, node_b) {
            var commit_a = $("[class*='branch_widget']", node_a);
            var commit_b = $("[class*='branch_widget']", node_b);
            var commit_count = (commit_a.length > 0 ? 1 : 0) +
                               (commit_b.length > 0 ? 1 : 0);

            if (commit_count == 2) {
                var b1 = commit_a.text().replace(/^\s+|\s+$/g, '');
                var b2 = commit_b.text().replace(/^\s+|\s+$/g, '');

                if (b1 == display_null_ref)
                    b1 = working_dir_request_token;

                if (b2 == display_null_ref)
                    b2 = working_dir_request_token;

                this.switch_commit_comp(b1, b2);
                return;
            }

            var file_a = $('> .file_widget', node_a);
            var file_b = $('> .file_widget', node_b);
            var file_count = (file_a.length > 0 ? 1 : 0) +
                             (file_b.length > 0 ? 1 : 0);

            if (commit_count > 0 && file_count > 0) {
                show_error({error: "Can't compare file and commit" });
                return;
            }

            if (file_count == 2) {
                var file1 = $('.path', file_a).text();
                var file2 = $('.path', file_b).text();
                var key1  = $('.key', file_a).text();
                var key2  = $('.key', file_b).text();

                if (key1 == display_null_ref)
                    key1 = working_dir_request_token;
                
                if (key2 == display_null_ref)
                    key2 = working_dir_request_token;

                this.switch_file_comp(key1, file1, key2, file2);
                return;
            }
        }
    };
})();

