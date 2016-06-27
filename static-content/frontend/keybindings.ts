/// <reference path="jquery.d.ts" />
/// <reference path="project.ts" />

type KeyBinding =
    { state: string
    , key: string[]
    , comment: string
    , func: () => void
    };

const keys : KeyBinding[] = [
  { state: 'keydown'
  , key: ['h']
  , comment: "Move selection left"
  , func: function() {
        Project.state.send_state_message({action: Project.GuiMessageCode.MOVE_LEFT});
    }
  },

  { state: 'keydown'
  , key: ['j', 'down']
  , comment: "Move selection down"
  , func: function(){
        Project.state.send_state_message({action: Project.GuiMessageCode.MOVE_DOWN});
    }
  },

  { state: 'keydown'
  , key: ['k', 'up']
  , comment: "Move selection up"
  , func: function() {
      Project.state.send_state_message({action: Project.GuiMessageCode.MOVE_UP});
    }
  },

  { state: 'keydown'
  , key: ['l']
  , comment: "Move selection right"
  , func: function() {
      Project.state.send_state_message({action: Project.GuiMessageCode.MOVE_RIGHT});
    }
  },

  { state: 'keydown'
  , key: ['0', 'Shift+0']
  , comment: "Move selection to the first element"
  , func: function(){
      Project.state.send_state_message({action: Project.GuiMessageCode.MOVE_FIRST});
    }
  },

  { state: 'keydown'
  , key: ['Shift+4']
  , comment: "Move selection to the last element"
  , func: function() {
      Project.state.send_state_message({action: Project.GuiMessageCode.MOVE_LAST});
    }
  },

  { state: 'keydown'
  , key: ['return']
  , comment: "Jump to selected element, or validate command line"
  , func: function() {
      Project.state.send_state_message({action: Project.GuiMessageCode.MOVE_INNER});
    }
  },

  { state: 'keydown'
  , key: [':', 's']
  , comment: "Open the command line mode"
  , func: function() {
      Project.state.send_state_message({action: Project.GuiMessageCode.COMMAND_REQUEST});
    }
  },

  { state: 'keyup'
  , key: ['esc']
  , comment: "Close command line mode"
  , func: function() {
      Project.state.send_state_message({action: Project.GuiMessageCode.ESCAPE});
    }
  },

  { state: 'keydown'
  , key: ['ctrl+left']
  , comment: "Go to previous breadcrumb"
  , func: function() {
      breadcrumb.go_backward();
    }
  },

  { state: 'keydown'
  , key: ['ctrl+right']
  , comment: "Go to next breadcrumb"
  , func: function(){
      breadcrumb.go_forward();
    }
  },

  { state: 'keyup'
  , key: ['1']
  , comment: "Set as left for comparison"
  , func: function() {
      Project.state.send_state_message({action: Project.GuiMessageCode.SELECT_AS_LEFT});
    }
  },

  { state: 'keyup'
  , key: ['2']
  , comment: "Set as right for comparison"
  , func: function() {
      Project.state.send_state_message({action: Project.GuiMessageCode.SELECT_AS_RIGHT});
    }
  },

  { state: 'keyup'
  , key: ['b']
  , comment: "Got to file blame"
  , func: function() {
      Project.state.send_state_message({action: Project.GuiMessageCode.SWITCH_BLAME});
    }
  }
];

function install_key_bindinges()
{
    for (let i = 0; i < keys.length; i++)
    {
        var def = keys[i];

        for (let j = 0; j < def.key.length; j++)
            $(document).bind(def.state, def.key[j], def.func);
    }
}

install_key_bindinges();
