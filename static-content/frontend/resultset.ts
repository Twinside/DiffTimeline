/// <reference path="project.ts" />

type GuiMessage = {action: string};

type GuiDescr =
	{ compact_view: boolean
	, fetch_previous: boolean
	, context_size: boolean
	, syntax_toggle: boolean };

interface ResultSet {
	create_all_dom: () => void;
	render_all: () => void;
	send_message: (msg: GuiMessage) => void;
	fetch_previous: (id: number) => void;
	gui_descr: GuiDescr;	
};

type DiffInfo =
    {way: Project.DiffChar, orig_idx: number, dest_idx: number, size: number};
	
type DiffRange = {way: Project.DiffChar, beg: number, end: number };
