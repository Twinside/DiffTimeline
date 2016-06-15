class subrange {
    beg: number;
    end: number;
}

type classname = string;

class PositionnalHighlighter {

    /** Array storing information about diff at the line
     * level (inner subdiff), Should be used by all node
     * creating function to interleave proper syntax
     * highlighting and inner diff.
     */
    private positionalHighlight : subrange[] = [];

    private currentPositionIndex: number = -1;

    private currentRealBeginning :number = -1;

    private currentEnd : number = -1;

    private huge_index : number = 999999;

    private previousIndex : number = 0;

    public reset() : void {
        this.currentPositionIndex = 0;
        this.previousIndex = 0;

        if (this.positionalHighlight.length > 0) {
            this.currentPositionIndex = 0;

            var curr_pos = this.positionalHighlight[this.currentPositionIndex];
            this.currentRealBeginning = curr_pos.beg;
            this.currentEnd = curr_pos.end;
        }
        else
        {
            this.currentPositionIndex = -1;
            this.currentRealBeginning = this.huge_index;
            this.currentEnd = this.huge_index;
        }
    }

    /** Update the state of the sub line highlighter,
     * update indices and detect end of boundary.
     */
    private use_to_position(pos : number) : void {
        var curr_pos : subrange =
            this.positionalHighlight[this.currentPositionIndex];

        if (pos < curr_pos.end) {
            this.currentRealBeginning = pos;
            return;
        }

        // we finished the current block, switch to the next
        // one.
        this.currentPositionIndex++;

        if (this.currentPositionIndex >= this.positionalHighlight.length)
            this.currentRealBeginning = this.huge_index; // really big value to avoid updating again.
        else {
            curr_pos = this.positionalHighlight[this.currentPositionIndex];
            this.currentRealBeginning = curr_pos.beg;
            this.currentEnd = curr_pos.end;
        }
    };

    /** return value <= 0 if no split is needed,
     *         value > n otherwise.
     */
    private index_splitter(index : number, size : number) : number {
        //                  hhhhhhhhhhhhhhh
        //      tttttt
        if (index + size < this.currentRealBeginning)
            return 0;

        var splitIndex = Math.min(index + size, this.currentEnd);

        this.use_to_position( splitIndex );

        return splitIndex;
    };

    public split_parts(producer : (a: string, b: string) => void, str : string) {
        var size = str.length;
        var curr_idx = this.previousIndex;
        var split = 1;

        while (size > 0)
        {
            var beg_distance = Math.min(size, Math.max(0, this.currentRealBeginning - curr_idx));

            if (curr_idx < this.currentRealBeginning && beg_distance > 0) {
                var beg = curr_idx - this.previousIndex;
                producer('', str.slice(beg, beg + beg_distance));
                curr_idx += beg_distance;
                size -= beg_distance;
            }

            if (size <= 0) break;

            split = this.index_splitter(curr_idx, size);

            if (split > 0) {
                var sub = str.slice(curr_idx - this.previousIndex, split - this.previousIndex);
                producer('sub', sub);
                size -= split - curr_idx;
                curr_idx = split;
            }
            
        } 

        this.previousIndex += str.length;
    };

    /** @type {function(!Array.<subrange>)} */
    public positional_setter(lst : Array<subrange>) {
        this.positionalHighlight  = lst;
    }
}

type RegionParser = (line: string, index : number) => number;
type SyntaxRecognizer = (line: string, index : number) => string;

/** @typedef ({kind: string, recognizer: function(string, number) : string}) */
type SyntaxParser = {kind: classname, recognizer: SyntaxRecognizer};

class KeywordTable {
    [key: string] : classname;
    public constructor() {
    }
    
    public static empty() : KeywordTable {
        return {};
    }  
}

interface LangDef {
    begin: RegionParser;
    end: RegionParser;
    kind: classname;
    parsers: SyntaxParser[];
    regions: LangDef[];
    keywords: KeywordTable;
}

interface LineHighlighter {
    colorLine: (line : string) => Node[];
    setPositionHighlight(lst : subrange[]);
    reset_context: () => void;
};


function context_free_highlight(kind : classname, txt : string) : Element {
    var span = document.createElement('span');
    var txtNode = document.createTextNode(txt);
    span.setAttribute('class', kind);
    span.appendChild(txtNode);
    return span
}

class EmptyHighlighter implements LineHighlighter {
    public with_line_number : boolean;
    protected pos_highlight = new PositionnalHighlighter();
    private current_line : number = 1;
    
    public constructor( with_line_number: boolean ) {
        this.with_line_number = with_line_number;
    }
    
    protected writeSubSplittedText(str : string ) : Node[] {
        var ret : Node[] = [];

        this.pos_highlight.split_parts(function( k, sub_str ) {
            if (k === '')
            ret.push(document.createTextNode(sub_str));
            else {
                var sub_span = document.createElement('span');
                sub_span.setAttribute('class', k);
                sub_span.appendChild(document.createTextNode(sub_str));
                ret.push(sub_span);
            }
        }, str);

        return ret;
    }
    
    public colorLine(line : string) : Node[] {
        this.pos_highlight.reset();

        var ret = this.writeSubSplittedText(line);
        var num = this.compute_line_number();
        if (num !== undefined)
            ret.unshift(num);

        return ret;
    }
     
    public setPositionHighlight(lst : subrange[]) {
        this.pos_highlight.positional_setter(lst);
    }
    
    public reset_context() {}   

    protected set_current_line_number(i : number) {
        this.current_line = i;
    }
    
    protected compute_line_number() : Element {
        this.current_line = this.current_line + 1;
        if (this.with_line_number) {
            return context_free_highlight('syntax_line_number', (this.current_line - 1).toString());
        }
        return undefined;
    }
}

class LanguageHighlighter extends EmptyHighlighter {
    private lang : LangDef;
    private activeStack : LangDef[];
    
    public constructor( with_line_number: boolean, lang: LangDef ) {
        super(with_line_number);
        this.lang = lang;
        this.activeStack = [lang];
    }
    
    public reset_context() {
        this.activeStack = [this.activeStack[0]];
    }

    private highlight(kind : classname, txt : string) : Element[] {
        var ret : Array<Element> = [];

        this.pos_highlight.split_parts(function(k, sub_str) {
            var span = document.createElement('span');
            var txtNode = document.createTextNode(sub_str);
            span.setAttribute('class', kind);
            span.appendChild(txtNode);

            if (k !== '') {
                var sub_span = document.createElement('span');
                sub_span.setAttribute('class', k);
                sub_span.appendChild(span);
                ret.push(sub_span);
            }
            else {
                ret.push(span);
            }

        }, txt);

        return ret;
    }
    
    public colorLine(line : string) : Node[] {
        var maxIndex = line.length;
        var consumed = false;
        var ret : Node[] = [];
        var textAccumulator = '';
        var that = this;

        this.pos_highlight.reset();

        var globNode = function(kind : string, tok : string) {
            var span = document.createElement('span');
            var maxi = ret.length;

            if (tok !== '') addText(tok);

            span.setAttribute('class', kind);

            for ( var i = 0; i < maxi; i++ )
                { span.appendChild(ret[i]); }

            ret = [span];
        };

        var flushText = function() {
            if (textAccumulator === '') return;
            var rez = that.writeSubSplittedText(textAccumulator );

            for (var i = 0; i < rez.length; i++)
                ret.push(rez[i]);

            textAccumulator = '';
        };

        var line_hi = function(kind : classname, txt : string) {
            flushText();
            return that.highlight(kind, txt);
        };

        var addNode = function(nodes : Node[]) {
            for ( var i = 0; i < nodes.length; i++ )
                ret.push(nodes[i]);
        };

        var addText = function(txt : string) {
            textAccumulator += txt;
        }

        var currentIndex = 0;

        while (currentIndex < maxIndex)
        {
            while (currentIndex < maxIndex &&
                   line[currentIndex] === ' ' || line[currentIndex] === '\t')
            { addText(line[currentIndex++]); }

            if (currentIndex >= maxIndex) break;

            var current_region = this.activeStack[ this.activeStack.length - 1 ];
            var consumed_chars = current_region.end(line, currentIndex)

            if (consumed_chars > 0)
            {
                if (this.activeStack.length > 1)
                    this.activeStack.pop();

                var to_add = textAccumulator;
                textAccumulator = '';
                var newNode = line_hi(current_region.kind,
                                      to_add + line.slice(currentIndex,
                                                          currentIndex + consumed_chars));
                addNode(newNode);
                currentIndex += consumed_chars;
                continue;
            }

            for ( var i in current_region.regions )
            {
                var r = current_region.regions[i];
                consumed_chars = r.begin(line, currentIndex);

                if (consumed_chars > 0)
                {
                    this.activeStack.push(r);
                    flushText();
                    addText(line.slice(currentIndex, currentIndex + consumed_chars));
                    consumed = true;
                    currentIndex += consumed_chars;
                    current_region = r;
                    break;
                }
            }

            if (currentIndex >= maxIndex) break;

            for ( var i in current_region.parsers )
            {
                var parser = current_region.parsers[i];
                var parserRet = parser.recognizer(line, currentIndex);

                if (parserRet !== '')
                {
                    if (this.lang.keywords.hasOwnProperty(parserRet))
                    {
                        var found_class = this.lang.keywords[parserRet]
                        addNode( line_hi(found_class, parserRet) );
                    }
                    else if (parser.kind !== '')
                        addNode( line_hi(parser.kind, parserRet) );
                    else
                        addText(parserRet);

                    currentIndex += parserRet.length;
                    consumed = true;
                    break;
                }
            }

            if (!consumed)
                { addText(line[currentIndex++]); }

            consumed = false;
        }

        flushText();

        // end of line, we must close all the
        // active tags (have to be reoppened a
        // the beginning
        for ( var j = this.activeStack.length - 1; j > 0; j-- ) {
            globNode(this.activeStack[j].kind, '');
        }

        var num = this.compute_line_number();
        if (num !== undefined)
            ret.unshift(num);
        return ret;
    };
}


/// Languages and parsers
namespace Languages {
    function isTokenPrefixOf( pref : string, line : string, base :number ) : boolean {
        if (line.length < pref.length || pref.length === 0) return false;

        for ( var i = 0; i < pref.length; i++ )
        {
            if (pref[i] !== line[i + base])
                return false;
        }

        return true;
    }
    
    function change_parser_kind(k : string, p : SyntaxParser) : SyntaxParser {
        return { kind: k, recognizer: p.recognizer };
    }

    function prefix_parser_kind(prefix : string, f : SyntaxParser)
        : (line: string, idx: number) => string {
        return function( line, idx ) {
            if (line[idx] !== prefix)
                return '';
            var sub = f.recognizer(line, idx + 1);
            if (sub === '') return '';
            return prefix + sub;
        };
    }

    function exact_token_match(k : string, txt : string) : SyntaxParser {
        return { kind: k, recognizer: function(line, idx) {
                if (isTokenPrefixOf(txt, line, idx))
                    return txt;
                return '';
            }
        };
    }

    namespace Parsers {
        export var integer : SyntaxParser = 
            { kind: 'syntax_number'
            , recognizer: function( line, idx ) {
                    var currIdx = idx;
                    while (currIdx < line.length && line[currIdx].match(/[0-9]/))
                        { currIdx++; }

                    return line.substring(idx, currIdx);
              }
            };

        export var c_like_identifier : SyntaxParser =
            { kind: ''
            , recognizer: function( line, idx ) {
                var currIdx = idx;

                if (currIdx < line.length && !line[currIdx++].match(/[a-zA-Z]/))
                    return '';

                while (currIdx < line.length && line[currIdx].match(/[_a-zA-Z0-9]/))
                    { currIdx++; }

                return line.substring(idx, currIdx);
              }
            };

        export var c_like_preproc : SyntaxParser =
            { kind: 'syntax_preproc'
            , recognizer: function( line, idx ) {
                    if (line[idx] !== '#') return '';
                    var currIdx = idx + 1;

                    while (currIdx < line.length && line[currIdx].match(/ \t/))
                        currIdx++;

                    if (currIdx >= line.length) return '';

                    while (currIdx < line.length && line[currIdx].match(/[a-zA-Z0-9_]/))
                        currIdx++;

                    return line.substring(idx, currIdx);
                }
            };

        export var simple_quote_string : SyntaxParser =
            { kind: 'syntax_string'
            , recognizer: function( line, idx ) {
                if (line[idx] !== "'") return '';
                var currIdx = idx + 1;

                while (currIdx < line.length)
                {
                    if (line[currIdx] === "'") {
                        return line.substring(idx, currIdx + 1);
                    }
                    currIdx++;
                }

                return '';
              }
            };

        export var double_quote_string : SyntaxParser =
            { kind: 'syntax_string'
            , recognizer: function( line, idx ) {
                if (line[idx] !== '"') return '';
                var currIdx = idx + 1;
                while (currIdx < line.length)
                {
                    if (line[currIdx] === '"') {
                        return line.substring(idx, currIdx + 1);
                    }
                    currIdx++;
                }

                return '';
              }
            };

        export var back_quote_string : SyntaxParser =
            { kind: 'syntax_special'
            , recognizer: function( line, idx ) {
                if (line[idx] !== '`') return '';
                var currIdx = idx + 1;
                while (currIdx < line.length)
                {
                    if (line[currIdx] === '`') {
                        return line.substring(idx, currIdx + 1);
                    }
                    currIdx++;
                }

                return '';
              }
            };

        export function monoline_comment( commentPrefix : string ) : SyntaxParser {
            return { kind:'syntax_comment'
                   , recognizer: function( line, idx ) {
                        return (isTokenPrefixOf( commentPrefix, line, idx ) 
                               ? line.substring(idx, line.length)
                               : '');
                   }
                }
        }
        
        
        export function tok_region(tok : string) : RegionParser {
            return function(line : string, base : number) : number {
                if ( isTokenPrefixOf(tok, line, base) )
                    return tok.length;
                else
                    return 0;
            }
        };
        
        export function multiline_comment( commentOpen: string, commentClose: string) : LangDef {
            return { begin:tok_region(commentOpen)
                   , end: tok_region(commentClose)
                   , kind:"syntax_comment"
                   , regions:[], parsers:[], keywords: KeywordTable.empty() }
        }
    }

    function expand_keyword_groups( lst : { kind: string, words: string[] }[] ) : KeywordTable {
        var ret = new KeywordTable();

        for ( var group in lst )
        {
            var g = lst[group];
            for ( var word in g.words )
                ret[g.words[word]] = g.kind;
        }

        return ret;
    }

    var null_region : RegionParser = function (line, base) { return 0; }

    function rexp_parser( k : string, rexp : RegExp ) : SyntaxParser {
        return {
            kind: k,
            recognizer: function( line, currIndex ) {
                rexp.lastIndex = currIndex;
                var rez = rexp.exec(line);

                if (rez !== null && rez.index === currIndex)
                    return rez[0];

                return '';
            }
        };
    }

    export const rubyDef : LangDef = {
        begin:null_region, end:null_region, kind: '',

        parsers:[ Parsers.monoline_comment('#')
                , Parsers.double_quote_string
                , Parsers.simple_quote_string
                , Parsers.integer
                , Parsers.c_like_identifier
                ],
            
        keywords: expand_keyword_groups(
            [ { kind:'syntax_preproc'  , words:["def", "class", "undef", "module", "end"] }
            , { kind:'syntax_keyword'  , words:["super", "yield", "alias", "undef" ] }
            , { kind:'syntax_bool'     , words:["true", "false"] }
            , { kind:'syntax_statement', words:[ "and", "break", "in", "next", "not", "or"
                                               , "redo", "rescue", "retry", "return"] }
            , { kind:'syntax_conditional', words: ["if", "case", "then", "else", "when", "elsif", "unless"] }
            , { kind:'syntax_repeat'     , words: ["while", "until", "for", "in"] }
            , { kind:'syntax_constant'   , words: ["nil", "self", "__FILE__", "__LINE__"] }
            ]),
        
        regions:[]
    };

    export const xmlDef : LangDef = {
        begin:null_region, end:null_region, kind: '',
        keywords: KeywordTable.empty(),
        parsers: [rexp_parser('syntax_special', /&[0-9A-Za-z]{1,8};/g)],
        regions:[Parsers.multiline_comment('<!--', '-->')
                ,{ begin:function(l, currentIndex) {
                            if (l[currentIndex] !== '<') return 0;
                            if (currentIndex + 1>= l.length) return 0;
                            if (l[currentIndex + 1].match(/[a-zA-Z]/))
                                return 1;

                            return 0;
                         }
                 , end: Parsers.tok_region(">")
                 , kind:"syntax_tag"
                 , regions:[]
                 , parsers:[ Parsers.double_quote_string
                           , Parsers.simple_quote_string
                           , change_parser_kind('syntax_tag_name', Parsers.c_like_identifier)
                           ]
                 , keywords:KeywordTable.empty()
                 }

                ,{ begin:function(l, currentIndex) {
                            if (l[currentIndex] !== '<') return 0;
                            if (currentIndex + 1>= l.length) return 0;
                            if (l[currentIndex + 1] !== '/') return 0;

                            return 2;
                         }

                 , end:Parsers.tok_region(">")
                 , kind:"syntax_tag"
                 , regions:[]
                 , parsers:[ change_parser_kind('syntax_tag_name', Parsers.c_like_identifier) ]
                 , keywords: KeywordTable.empty()
                 }
                 ]
    };

    export const cssDef : LangDef = {
        begin:null_region, end:null_region, kind: '',

        regions:[Parsers.multiline_comment('/*', '*/')
                ,{ begin: Parsers.tok_region("{")
                 , end: Parsers.tok_region("}")
                 , kind:""
                 , regions:[]
                 , parsers:
                    [ Parsers.double_quote_string
                    , rexp_parser( 'syntax_storage_class', /[a-zA-Z][a-zA-Z0-9-]*\s*(?=:)/g )
                    , rexp_parser( 'syntax_number',
                                   /[-+]?\d+(\.\d*)?(%|mm|cm|in|pt|pc|em|ex|px)/g )
                    , rexp_parser( 'syntax_constant', /#[0-9a-fA-F]{6}/g )
                    , rexp_parser( 'syntax_constant', /#[0-9a-fA-F]{3}/g )
                    ]
                 , keywords: KeywordTable.empty()
                 }
                ],

        parsers:[ { kind:'syntax_structure'
                  , recognizer:prefix_parser_kind('.', Parsers.c_like_identifier )}
                , { kind:'syntax_function'
                  , recognizer:prefix_parser_kind('#', Parsers.c_like_identifier )}
                , { kind:'syntax_preproc'
                  , recognizer:prefix_parser_kind(':', Parsers.c_like_identifier )}
                , Parsers.c_like_identifier
                ],
            
        keywords:expand_keyword_groups(
            [ { kind:'syntax_statement'
              , words: [ "abbr", "acronym", "address", "applet", "area", "a", "b"
                       , "base", "basefont", "bdo", "big", "blockquote", "body"
                       , "br", "button", "caption", "center", "cite", "code", "col"
                       , "colgroup", "dd", "del", "dfn", "dir", "div", "dl", "dt"
                       , "em", "fieldset", "font", "form", "frame", "frameset", "h1"
                       , "h2", "h3", "h4", "h5", "h6", "head", "hr", "html", "img"
                       , "i", "iframe", "img", "input", "ins", "isindex", "kbd"
                       , "label", "legend", "li", "link", "map", "menu", "meta"
                       , "noframes", "noscript", "ol", "optgroup", "option", "p"
                       , "param", "pre", "q", "s", "samp", "script", "select", "small"
                       , "span", "strike", "strong", "style", "sub", "sup", "tbody"
                       , "td", "textarea", "tfoot", "th", "thead", "title", "tr"
                       , "tt", "ul", "u", "var", "table"
                       ] }
            ])
    };

    export const shellDef : LangDef = {
        begin:null_region, end:null_region, kind: '',
        parsers:[ Parsers.monoline_comment('#')
                , Parsers.double_quote_string
                , Parsers.simple_quote_string
                , Parsers.back_quote_string
                , Parsers.integer
                , rexp_parser('syntax_identifier', /[a-zA-Z0-9.!@_%+,]*(?==)/g)
                , rexp_parser('syntax_operator', /<<|>>|!=|==|\||\&>/g)
                , exact_token_match('syntax_identifier', '$#')
                , { kind: 'syntax_identifier'
                  , recognizer:prefix_parser_kind('$', Parsers.c_like_identifier )}
                , Parsers.c_like_identifier
                ],
            
        keywords: expand_keyword_groups(
            [ { kind:'syntax_conditional', words: ["if", "else", "elif", "fi", "then", "case", "esac"] }
            , { kind:'syntax_repeat'     , words: ["while", "until", "do", "for", "done", "in"] }
            , { kind:'syntax_function'   , words: ["function"] }
            ]),
        
        regions:[]
    };

    export const graphvizDef : LangDef = {
        begin:null_region, end:null_region, kind: '',

        parsers:[ Parsers.monoline_comment('#')
                , Parsers.double_quote_string
                , Parsers.simple_quote_string
                , Parsers.integer
                , Parsers.c_like_identifier
                ],
            
        keywords: expand_keyword_groups(
            [ { kind:'syntax_operator' , words:["->"] }
            , { kind:'syntax_statement', words:["node","digraph", "subgraph", "edge"]}
            ]),

        regions:[]
    }

    export const pythonDef : LangDef = {
        begin:null_region, end:null_region, kind: '',

        parsers:[ Parsers.monoline_comment('#')
                , Parsers.double_quote_string
                , Parsers.simple_quote_string
                , Parsers.integer
                , Parsers.c_like_identifier
                ],
            
        keywords: expand_keyword_groups(
            [ { kind:'syntax_preproc'  , words:["from", "import"] }
            , { kind:'syntax_bool'     , words:["True", "False"] }
            , { kind:'syntax_statement', words: ["None","as", "assert", "break"
                                                ,"continue", "del", "exec", "global", "lambda"
                                                ,"nonlocal", "pass", "print", "return", "with"
                                                ,"yield", "class", "def"]}
            , { kind:'syntax_conditional', words: ["if", "else", "elif"] }
            , { kind:'syntax_repeat'     , words: ["while", "for"] }
            , { kind:'syntax_exception', words:["except", "finally", "raise", "try"] }
            ]),

        regions:[
            { begin:Parsers.tok_region('"""')
            , end:Parsers.tok_region('"""')
            , kind:"syntax_string"
            , regions:[], parsers:[], keywords: KeywordTable.empty() }
        ]
    };

    export var makefileDef : LangDef = {
        begin:null_region, end:null_region, kind: '',

        parsers:[ Parsers.monoline_comment('#')
                 // target
                , rexp_parser('syntax_function', /^[A-Za-z0-9_./$()%-][A-Za-z0-9_./\t $()%-]*::?/g)
                , Parsers.c_like_identifier
                ],

        keywords: expand_keyword_groups(
            [ { kind:'syntax_preproc'  , words:["from", "import"] }
            , { kind:'syntax_statement'
              , words: [ 'subst', 'abspath', 'addprefix', 'addsuffix', 'and',
                         'basename', 'call', 'dir', 'error', 'eval',
                         'filter-out', 'filter', 'findstring', 'firstword', 'flavor',
                         'foreach', 'if', 'info', 'join', 'lastword',
                         'notdir', 'or', 'origin', 'patsubst', 'realpath',
                         'shell', 'sort', 'strip', 'suffix', 'value',
                         'warning', 'wildcard', 'word', 'wordlist', 'words' ]}
            ]),

        regions:[]
    };

    function make_region_recursive(region : LangDef) : LangDef {
        region.regions.push(region);
        return region;
    }

    export const ocamlDef : LangDef = {
        begin:null_region, end:null_region, kind: '',

        regions: [make_region_recursive(Parsers.multiline_comment('(*', '*)'))],

        parsers:[ rexp_parser('syntax_structure', /[A-Z][A-Za-z0-9_]*\./g)
                , rexp_parser('syntax_constant', /[A-Z][A-Za-z0-9_]*/g)
                , Parsers.c_like_identifier
                , Parsers.double_quote_string
                , Parsers.integer
                ],

        keywords: expand_keyword_groups(
            [ { kind:'syntax_conditional', words: ['if', 'then', 'else'] }
            , { kind:'syntax_structure', words:['module', 'sig'] }
            , { kind:'syntax_repeat', words:['downto', 'to', 'for'] }
            , { kind:'syntax_preproc', words:['import'] }
            , { kind:'syntax_keyword', words:['and', 'as', 'assert', 'class', 'constraint'
                                             ,'exception', 'external', 'fun', 'in'
                                             ,'inherit', 'initializer', 'land', 'lazy'
                                             ,'let', 'match', 'method', 'mutable', 'new'
                                             ,'of', 'parser', 'private', 'raise', 'rec', 'end'
                                             ,'done', 'open', 'val', 'begin', 'function',
                                             ,'try', 'type', 'virtual', 'when', 'while', 'with'] }
            , { kind:'syntax_bool'     , words:["true", "false"] }
            , { kind:'syntax_operator', words:['asr', 'lnot', 'lor', 'lsl', 'lsr'
                                              ,'lxor', 'mod', 'not'] }

            , { kind:'syntax_type'
              , words:['array', 'bool', 'char', 'exn', 'float',
                       'format', 'format4', 'int', 'int32', 'int64',
                       'lazy_t', 'list', 'nativeint', 'option',
                       'string', 'unit']}

            ])
    };
    
    export const haskellDef : LangDef = {
        begin:null_region, end:null_region, kind: '',

        regions: [make_region_recursive(Parsers.multiline_comment('{-', '-}'))],
        parsers:[ Parsers.c_like_identifier
                , Parsers.monoline_comment('--')
                , Parsers.double_quote_string
                , Parsers.integer
                ],

        keywords: expand_keyword_groups(
            [ { kind:'syntax_conditional', words: ['if', 'then', 'else'] }
            , { kind:'syntax_statement', words:['do', 'case', 'of', 'let', 'in'  ] }
            , { kind:'syntax_structure', words:['module'] }
            , { kind:'syntax_preproc', words:['import'] }
            , { kind:'syntax_structure'
              , words: ['data', 'family', 'class', 'where', 'instance', 'default'
                       ,'deriving']  }
            , { kind:'syntax_typedef', words: ['type', 'newtype'] }
            , { kind:'syntax_bool'     , words:["True", "False"] }
            , { kind:'syntax_type'
              , words:[ 'Int', 'Integer', 'Char', 'Bool', 'Float'
                      , 'Double', 'IO', 'Void', 'Addr', 'Array'
                      , 'String', 'Maybe', 'Either', 'Ratio', 'Complex'
                      , 'Ordering', 'IOError', 'IOResult', 'ExitCode']}
            ])
    };

    export const javascriptDef : LangDef = {
        begin:null_region, end:null_region, kind: '',

        regions:[Parsers.multiline_comment('/*', '*/')],

        parsers:[ Parsers.c_like_identifier
                , Parsers.double_quote_string
                , Parsers.simple_quote_string
                , Parsers.integer
                , Parsers.monoline_comment('//')
                ],

        keywords:expand_keyword_groups(
            [ { kind:'syntax_conditional', words: ['if', 'else', 'switch'] }
            , { kind:'syntax_repeat', words:['while', 'for', 'do', 'in'] }
            , { kind:'syntax_statement', words: ['return', 'with'] }
            , { kind:'syntax_label', words:['case', 'default', 'break', 'continue'] }
            , { kind:'syntax_exception', words:["throw", "try", "catch", "finally"] }
            , { kind:'syntax_bool'     , words:["true", "false"] }
            , { kind:'syntax_operator' , words:['new', 'delete', 'instanceof', 'typeof'] }
            , { kind:'syntax_function' , words:['function'] }

            , { kind:'syntax_identifier', words:['arguments', 'this', 'var', 'let'] }
            , { kind:'syntax_type'
              , words:['Array', 'Boolean', 'Date', 'Function', 'Number', 'Object', 'String', 'RegExp']
              }
            ])
    };

    export const phpDef : LangDef = {
        begin:null_region, end:null_region, kind: '',

        regions:[Parsers.multiline_comment('/*', '*/')],
        
        parsers:[ { kind:'syntax_identifier'
                  , recognizer:prefix_parser_kind('$', Parsers.c_like_identifier )}
                , Parsers.double_quote_string
                , Parsers.integer
                , Parsers.monoline_comment('//')
                ],

        keywords:expand_keyword_groups(
            [ { kind:'syntax_conditional'
              , words: ['declare', 'else', 'enddeclare', 'endswitch', 'elseif'
                       , 'endif', 'if', 'switch'] }

            , { kind:'syntax_repeat'
              , words:['as', 'do', 'endfor', 'endforeach', 'endwhile', 'for'
                      ,'foreach', 'while'] }

            , { kind:'syntax_statement'
              , words: ['return', 'break', 'continue', 'exit', 'goto', 'die'] }

            , { kind:'syntax_bool', words:["true", "false"] }
            , { kind:'syntax_define'
              , words: ['new', 'clone']}

            , { kind:'syntax_function'
              , words: ['function'] }

            , { kind:'syntax_label', words:['case', 'default', 'switch'] }
            , { kind:'syntax_keyword', words: ['var', 'const'] }

            , { kind:'syntax_type'
              , words: ['boolean', 'bool', 'integer', 'int', 'real', 'double'
                       ,'float', 'string', 'array', 'object', 'NULL'] }

            , { kind:'syntax_structure'
              , words: ['namespace', 'extends', 'implements', 'instanceof'
                       ,'parent', 'self', 'class', 'interface'] }

            , { kind:'syntax_exception'
              , words: ['catch', 'throw', 'try'] }

            , { kind:'syntax_storage_class'
              , words: ['final', 'global', 'private', 'protected', 'public', 'static'] }

            , { kind:'syntax_constant'
              , words:['PHP_VERSION',  'PHP_OS',  'DEFAULT_INCLUDE_PATH',  'PEAR_INSTALL_DIR'
                      ,'PEAR_EXTENSION_DIR',  'PHP_EXTENSION_DIR',  'PHP_BINDIR',  'PHP_LIBDIR'
                      ,'PHP_DATADIR',  'PHP_SYSCONFDIR',  'PHP_LOCALSTATEDIR'
                      ,'PHP_CONFIG_FILE_PATH',  'PHP_OUTPUT_HANDLER_START',  'PHP_OUTPUT_HANDLER_CONT'
                      ,'PHP_OUTPUT_HANDLER_END',  'E_ERROR',  'E_WARNING',  'E_PARSE',  'E_NOTICE'
                      ,'E_CORE_ERROR',  'E_CORE_WARNING',  'E_COMPILE_ERROR',  'E_COMPILE_WARNING'
                      ,'E_USER_ERROR',  'E_USER_WARNING',  'E_USER_NOTICE',  'E_ALL']
              }
            ])
    };

    export const modelicaDef : LangDef = {
        begin:null_region, end:null_region, kind: '',

        regions:[Parsers.multiline_comment('/*', '*/')],

        parsers:[ Parsers.c_like_identifier
                , Parsers.c_like_preproc
                , Parsers.double_quote_string
                , Parsers.integer
                , Parsers.monoline_comment('//')
                ],
            
        keywords:expand_keyword_groups(
            [ { kind:'syntax_conditional', words: ['if', 'else', 'then', 'elseif', 'when', 'elsewhen'] }
            , { kind:'syntax_statement', words: ['pre', 'edge', 'initial'] }
            , { kind:'syntax_label', words:[] }
            , { kind:'syntax_repeat', words:[] }
            , { kind:'syntax_structure', words: ['model', 'record', 'end', 'equation'] }
            , { kind:'syntax_storage_class'
              , words: ['external', 'connector', 'input', 'output' ] }
            , { kind:'syntax_type'
              , words:['Integer', 'Real', 'Boolean']
              }
            ])
    };
    
    const cKeywords : any[] =
        [ { kind:'syntax_conditional', words: ['if', 'else', 'switch'] }
        , { kind:'syntax_statement', words: ['goto', 'break', 'return', 'continue', 'asm'] }
        , { kind:'syntax_label', words:['case', 'default'] }
        , { kind:'syntax_repeat', words:['while', 'for', 'do'] }
        , { kind:'syntax_structure', words: ['struct', 'union', 'enum', 'typedef'] }
        , { kind:'syntax_storage_class'
            , words: ['static', 'register', 'auto', 'volatile',
                    'extern', 'const', 'inline'] }
        , { kind:'syntax_type'
            , words:[ 'int', 'long', 'short', 'char', 'void', 'signed', 'unsigned'
                    , 'float', 'double', 'size_t', 'ssize_t', 'off_t', 'wchar_t'
                    , 'ptrdiff_t', 'sig_atomic_t', 'fp2408.339os_t', 'clock_t', 'time_t'
                    , 'va_list', 'jmp_buf', 'FILE', 'DIR', 'div_t', 'ldiv_t'
                    , 'mbstate_t', 'wctrans_t', 'wint_t', 'wctype_t', 'bool'
                    , 'complex', 'int8_t', 'int16_t', 'int32_t', 'int64_t'
                    , 'uint8_t', 'uint16_t', 'uint32_t', 'uint64_t', 'int_least8_t'
                    , 'int_least16_t', 'int_least32_t', 'int_least64_t'
                    , 'uint_least8_t', 'uint_least16_t', 'uint_least32_t'
                    , 'uint_least64_t', 'int_fast8_t', 'int_fast16_t'
                    , 'int_fast32_t', 'int_fast64_t', 'uint_fast8_t'
                    , 'uint_fast16_t', 'uint_fast32_t', 'uint_fast64_t', 'intptr_t'
                    , 'uintptr_t', 'intmax_t', 'uintmax_t', '__label__'
                    , '__complex__', '__volatile__']
            }
        ];
            
    export const cDef : LangDef = {
        begin:null_region, end:null_region, kind: '',
        regions:[Parsers.multiline_comment('/*', '*/')],
        parsers:[ Parsers.c_like_identifier
                , Parsers.c_like_preproc
                , Parsers.double_quote_string
                , Parsers.integer
                , Parsers.monoline_comment('//')
                ],
        keywords:expand_keyword_groups(cKeywords)
    };

    const cppKeywords =
        [ { kind:'syntax_statement', words:['new', 'delete', 'this', 'friend', 'using'] }
        , { kind:'syntax_statement', words:["public", "protected", "private"] }
        , { kind:'syntax_bool'     , words:["true", "false"] }
        , { kind:'syntax_type', words:["inline", "virtual", "explicit", "export", "bool", "wchar_t"] }
        , { kind:'syntax_exception', words:["throw", "try", "catch"] }
        , { kind:'syntax_operator', words:["operator", "typeid", "and", "bitor", "or", "xor"
                                            ,"compl" ,"bitand", "and_eq", "or_eq", "xor_eq", "not"
                                            , "not_eq"]}
        , { kind:'syntax_structure', words:["class", "typename", "template", "namespace"] }
        ];
        
    export const cppDef : LangDef = {
        begin:null_region, end:null_region, kind: '',
        regions:[Parsers.multiline_comment('/*', '*/')],
        parsers:[ Parsers.c_like_identifier
                , Parsers.c_like_preproc
                , Parsers.double_quote_string
                , Parsers.integer
                , Parsers.monoline_comment('//')
                ],
        keywords:expand_keyword_groups(cKeywords.concat(cppKeywords))
    };

    export const csDef : LangDef = {
        begin:null_region, end:null_region, kind: '',

        regions: cDef.regions,
        parsers:[ Parsers.c_like_identifier
                , Parsers.c_like_preproc
                , Parsers.double_quote_string
                , Parsers.integer
                , Parsers.monoline_comment('//')
                ],
            
        keywords:expand_keyword_groups(
            [ { kind:'syntax_conditional', words: ['if', 'else', 'switch'] }
            , { kind:'syntax_statement', words: ['goto', 'break', 'return', 'continue', 'asm'] }
            , { kind:'syntax_label', words:['case', 'default'] }
            , { kind:'syntax_repeat', words:['while', 'for', 'foreach', 'do'] }
            , { kind:'syntax_structure', words: ['struct', 'union', 'enum', 'class'] }
            , { kind:'syntax_storage_class'
              , words: ['static', 'auto', 'const'] }
            , { kind:'syntax_type'
              , words:[ 'int', 'long', 'short', 'char', 'void', 'signed', 'unsigned'
                      , 'float', 'double', 'size_t', 'ssize_t', 'off_t', 'wchar_t'
                      , 'ptrdiff_t', 'sig_atomic_t', 'fp2408.339os_t', 'clock_t', 'time_t'
                      , 'va_list', 'jmp_buf', 'FILE', 'DIR', 'div_t', 'ldiv_t'
                      , 'mbstate_t', 'wctrans_t', 'wint_t', 'wctype_t', 'bool'
                      , 'complex', 'int8_t', 'int16_t', 'int32_t', 'int64_t'
                      , 'uint8_t', 'uint16_t', 'uint32_t', 'uint64_t', 'int_least8_t'
                      , 'int_least16_t', 'int_least32_t', 'int_least64_t'
                      , 'uint_least8_t', 'uint_least16_t', 'uint_least32_t'
                      , 'uint_least64_t', 'int_fast8_t', 'int_fast16_t'
                      , 'int_fast32_t', 'int_fast64_t', 'uint_fast8_t'
                      , 'uint_fast16_t', 'uint_fast32_t', 'uint_fast64_t', 'intptr_t'
                      , 'uintptr_t', 'intmax_t', 'uintmax_t', '__label__'
                      , '__complex__', '__volatile__']
              }
            ])
    };
    
    function assoc(expr : RegExp, lang : LangDef) {
        return {pattern: expr, def: lang};
    };

    export const langAssoc = [
        assoc(/\.rb$/      , rubyDef),
        assoc(/\.c$/       , cDef),
        assoc(/\.cpp$/     , cppDef),
        assoc(/\.cc$/      , cppDef),
        assoc(/\.h$/       , cppDef),
        assoc(/\.hpp$/     , cppDef),
        assoc(/\.cs$/      , csDef),
        assoc(/\.php$/     , phpDef),
        assoc(/\.ml$/      , ocamlDef),
        assoc(/\.mli$/     , ocamlDef),
        assoc(/\.mll$/     , ocamlDef),
        assoc(/\.mly$/     , ocamlDef),
        assoc(/\.mo$/      , modelicaDef),
        assoc(/\.php3$/    , phpDef),
        assoc(/\.hs$/      , haskellDef),
        assoc(/\.css$/     , cssDef),
        assoc(/\.py$/      , pythonDef),
        assoc(/\.js$/      , javascriptDef),
        assoc(/\.as$/      , javascriptDef),
        assoc(/\.mxml$/    , xmlDef),
        assoc(/\.sh$/      , shellDef),
        assoc(/\.xml$/     , xmlDef),
        assoc(/\.html?$/   , xmlDef),
        assoc(/\.vcxproj$/ , xmlDef),
        assoc(/\.gz$/      , graphvizDef),
        assoc(/\.dot$/     , graphvizDef),
        assoc(/[mM]akefile$/, makefileDef)
    ];
}

class TinySyntaxHighlighter {
    private activeStack : LangDef[];
    private lang : LangDef;
    private set_current_line_number : (i: number) => void;
    private compute_line_number : () => Element;

    private nextSpaceIndex(line : string, idx: number ) : number {
        var spaceIndex = line.indexOf(' ', idx );
        var tabIndex = line.indexOf('\t', idx );
        return Math.min( spaceIndex, tabIndex );
    }

    private html_encodize(snipp : string) : string {
        return snipp.replace(/\&/g, '\&amp;').replace(/</g, '\&lt;').replace(/</g, '\&gt;');
    }

    public static from_filename(with_line_number : boolean, filename : string) : LineHighlighter
    {
        var i;
        var langAssoc = Languages.langAssoc;

        for (i = 0; i < langAssoc.length; i++)
        {
            if (filename.match(langAssoc[i].pattern))
                return new LanguageHighlighter(with_line_number, langAssoc[i].def);
        }

        return new EmptyHighlighter(with_line_number);
    }

    public highlight_node(with_line_number : boolean, filename : string, node : Element)
    {
        var highlighter =
            this.from_filename(with_line_number, filename);

        var text = node.textContent;
        var lines = text.split('\n');

        node.textContent = "";

        for (var i = 0; i < lines.length; i++)
        {
            var line = highlighter.colorLine(lines[i]);
            for (var j = 0; j < line.length; j++)
                node.appendChild(line[j]);

            node.appendChild(document.createTextNode('\n'));
        }
    }
}
