/// <reference path="resultset.ts" />
/// <reference path="breadcrumb.ts" />
/// <reference path="jquery.d.ts" />
/// <reference path="global_constant.ts" />

namespace Project {
	export enum ViewMode {
		VIEW_FULL = 1,
		VIEW_COMPACT = 0
	}
	
	export class DiffChar {
		public static DIFF_ADD = '+';
		public static DIFF_DEL = '-';
		public static DIFF_DELADD = '~';
		public static DIFF_ADDDEL = '!';
	}
	
	export enum GuiMessage {
		FETCH_TREE =   0,
		FETCH_DETAIL = 1,
		MOVE_LEFT =    2,
		MOVE_RIGHT =   3,
		MOVE_UP =      4,
		MOVE_DOWN =    5,
		MOVE_FIRST =   6,
		MOVE_LAST =    7,
	
		MOVE_INNER =      8,
		COMMAND_REQUEST = 9,
		ESCAPE =          10,
		SELECT_AS_LEFT =  11,
		SELECT_AS_RIGHT = 12,
		SWITCH_BLAME =    13
	}
	
	export const DiffKind = {
		KIND_MODIFICATION: 'modification',
		KIND_ADDITION: 'addition',
		KIND_DELETION: 'deletion'
	}

	export class State {
		private view_mode : ViewMode = ViewMode.VIEW_FULL;
    	private apply_syntax_coloration = true;
        private context_size : number = 2;
        private max_commit_delta_show : number = 15;
        private states : ResultSet[] = [];
        private forward_states : ResultSet[] = [];
        private commit_delta_margin : number = 6;
        private apparition_duration_time : number = 750;
        private apparition_pane_duration : number = 500;
    	private btn_toggle_text : any = {}

        public constructor() {
            this.btn_toggle_text[Project.ViewMode.VIEW_FULL] =  '&#x25bc;<br/>&#x25b2;';
            this.btn_toggle_text[Project.ViewMode.VIEW_COMPACT] =  '&#x25b2;<br/>&#x25bc;';
        }
   
        private show_hide_toolbar_elements(descr : GuiDescr) {
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
        }

        public active_view_mode() : ViewMode { return this.view_mode; }
        public active_context_size() { return this.context_size; }
        public apparition_duration() { return this.apparition_duration_time; }

        public send_state_message(message : GuiMessage) {
            const last_path = this.states[ this.states.length - 1 ];
            last_path.send_message(message);
	    }

	    public create_all_dom() {
            var last_path = this.states[ this.states.length - 1 ];
            this.show_hide_toolbar_elements(last_path.gui_descr);
            last_path.create_all_dom();
        }

        public render_all() {
            var last_path = this.states[ this.states.length - 1 ];
            last_path.render_all();
        }

        public set_previous_button_count( count : number, tooltips: string[] ) {
            const container = $('div .return_past_container');
            container.children().remove();

            for (let i = 0; i < count; i++) {
                var tooltip = i < tooltips.length ? tooltips[i] : "";
                var sub_node = ich.fetch_previous(
                    {id: i, tooltip: tooltip}
                );
                $(container).append( sub_node[0] );
            }
        }

        public get_previous(id : number) {
            var last_path = this.states[ this.states.length - 1 ];
            last_path.fetch_previous(id);
        }

        public switch_blame(start_commit : ref , file : string) {
            this.clear_display();
            BlameShower.create_from_arg(start_commit, file, function( new_state ) {
                this.states.push( new_state );
                this.show_hide_toolbar_elements(new_state.gui_descr);
                breadcrumb.append_breadcrumb("Blame (" + file + ")");
            });
        }

        public switch_file(file : string, fkey : ref, start_commit : ref) {
            this.clear_display();

            if (start_commit == display_null_ref)
                start_commit = working_dir_request_token;

            FileRenderer.create_from_arg(file, fkey, start_commit,
                                         function( new_state ) {
                this.states.push( new_state );
                this.show_hide_toolbar_elements(new_state.gui_descr);
                breadcrumb.append_breadcrumb(file);
            });
        }

        public switch_commit_comp( b1, b2 ) {
            this.clear_display();
            var new_state = CommitComparer.create_from_args(b1, b2);
            this.states.push( new_state );
            this.show_hide_toolbar_elements(new_state.gui_descr);
            breadcrumb.append_breadcrumb('Compare commit');
        }

        public switch_file_comp(key1, file1, key2, file2) {
            this.clear_display();
            const new_state = FileComparer.create_from_args(key1, file1, key2, file2);
            this.states.push( new_state );
            this.show_hide_toolbar_elements(new_state.gui_descr);
            breadcrumb.append_breadcrumb('Compare file');
        }

        public switch_commit(id : ref) {
            this.clear_display();
            const new_state = new CommitRenderer.create_from_arg(id);
            this.states.push(new_state);

            if (id !== null_ref)
                breadcrumb.append_breadcrumb(id);
            else
                breadcrumb.append_breadcrumb('HEAD');

            this.show_hide_toolbar_elements(new_state.gui_descr);
        }

        public jump_context(idx : number) {
            if (this.states.length - 1 === idx) return;

            if (idx < this.states.length - 1) {
                while (idx != this.states.length - 1)
                    this.forward_states.push(this.states.pop());
            } else {
                while (idx != this.states.length - 1)
                    this.states.push(this.forward_states.pop());
            }

            this.clear_display();
            this.create_all_dom();
            this.render_all();
            /* TODO restore button count */
        }

        public clear_display() {
            this.set_previous_button_count(1, []);
            $('.container > *').remove();
        }

        public increase_context_size() {
            this.context_size = this.context_size + 1;
            $('.toolbar div textarea').text(this.context_size.toString());

            if (this.view_mode === Project.ViewMode.VIEW_COMPACT)
                { this.render_all(); }
        }

        public decrease_context_size() {
            this.context_size = Math.max(0, this.context_size - 1);
            $('.toolbar div textarea').text(this.context_size.toString());

            if (this.view_mode === Project.ViewMode.VIEW_COMPACT)
                { this.render_all(); }
        }

        public toggle_diff_full() {
            if (this.view_mode === Project.ViewMode.VIEW_FULL)
                this.view_mode = Project.ViewMode.VIEW_COMPACT;
            else
                this.view_mode = Project.ViewMode.VIEW_FULL;

            $('.btn_toggleview').html(this.btn_toggle_text[this.view_mode]);
            this.render_all();
        }

        public chrome_scroll_offset() {
            return {top:-120, left:-120};
        }

        public start_blame(blame_obj) {
            var new_state = BlameShower.create_from_data(blame_obj);
            this.states.push( new_state );
            breadcrumb.append_breadcrumb('Blame (' + blame_obj.filename + ')');
            this.show_hide_toolbar_elements(new_state.gui_descr);
        }

        public start_commit(commit_obj) {
            var new_state = CommitRenderer.create_from_data(commit_obj);
            this.states.push( new_state );
            this.show_hide_toolbar_elements(new_state.gui_descr);

            if (commit_obj.key !== null_ref)
                breadcrumb.append_breadcrumb(commit_obj.key);
            else
                breadcrumb.append_breadcrumb('HEAD');
        }

        public start_file(file_obj) {
            const new_state = FileRenderer.create_from_data(file_obj);
            this.states.push( new_state );
            this.show_hide_toolbar_elements(new_state.gui_descr);
            breadcrumb.append_breadcrumb(file_obj.filename);
        }

        public update_diff() {
            const zone_a = $('.global_compare_recipient_a');
            const zone_b = $('.global_compare_recipient_b');

            Project.state.check_comparison(zone_a, zone_b);
        }

        public check_comparison(node_a : Node, node_b : Node) {
            const commit_a = $("[class*='branch_widget']", node_a);
            const commit_b = $("[class*='branch_widget']", node_b);
            const commit_count = (commit_a.length > 0 ? 1 : 0) + (commit_b.length > 0 ? 1 : 0);

            if (commit_count === 2) {
                let b1 = commit_a.text().replace(/^\s+|\s+$/g, '');
                let b2 = commit_b.text().replace(/^\s+|\s+$/g, '');

                if (b1 === display_null_ref)
                    b1 = working_dir_request_token;

                if (b2 === display_null_ref)
                    b2 = working_dir_request_token;

                this.switch_commit_comp(b1, b2);
                return;
            }

            const file_a = $('> .file_widget', node_a);
            const file_b = $('> .file_widget', node_b);
            const file_count = (file_a.length > 0 ? 1 : 0) + (file_b.length > 0 ? 1 : 0);

            if (commit_count > 0 && file_count > 0) {
                show_error({error: "Can't compare file and commit" });
                return;
            }

            if (file_count === 2) {
                const file1 = $('.path', file_a).text();
                const file2 = $('.path', file_b).text();
                let key1 = $('.key', file_a).text();
                let key2 = $('.key', file_b).text();

                if (key1 == display_null_ref)
                    key1 = working_dir_request_token;
                
                if (key2 == display_null_ref)
                    key2 = working_dir_request_token;

                this.switch_file_comp(key1, file1, key2, file2);
                return;
            }
        }
    };

    export var state : State = new State();
}