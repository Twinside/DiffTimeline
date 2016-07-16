
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
	
	export enum GuiMessageCode {
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
}
