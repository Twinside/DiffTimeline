// meh, there is no real type for that
/** @typedef (Object) */
var regexp;

/** @typedef ({beg:number, end:number}) */
var subrange;

/** @typedef ({positional_setter:function(Array.<subrange>),
 *                         reset:function(),
 *                   split_parts:function(function(string, string), string)}) */
var SubHighlighter;

/** @typedef (string) */
var classname;

/**
 * @type {function(new:SubHighlighter)}
 */
var PositionnalHighlighter = function() {
    "use strict";

    /** Array storing information about diff at the line
     * level (inner subdiff), Should be used by all node
     * creating function to interleave proper syntax
     * highlighting and inner diff.
     * @type {!Array.<subrange>}
     */
    var positionalHighlight = [];

    /** @type {number} */
    var currentPositionIndex = -1;

    /** @type {number} */
    var currentRealBeginning = -1;

    /** @type {number} */
    var currentEnd = -1;

    /** @type {number} */
    var huge_index = 999999;

    /** @type {number} */
    var previousIndex = 0;

    /** @type {function()} */
    var inner_reset = function() {
        currentPositionIndex = 0;
        previousIndex = 0;

        if (positionalHighlight.length > 0) {
            currentPositionIndex = 0;

            var curr_pos = positionalHighlight[currentPositionIndex];
            currentRealBeginning = curr_pos.beg;
            currentEnd = curr_pos.end;
        }
        else {
            currentPositionIndex = -1;
            currentRealBeginning = huge_index;
            currentEnd = huge_index;
        }
    }

    /** Update the state of the sub line highlighter,
     * update indices and detect end of boundary.
     * @type {function(number)}
     */
    var use_to_position = function( pos ) {
        var curr_pos = positionalHighlight[currentPositionIndex];

        if (pos < curr_pos.end) {
            currentRealBeginning = pos;
            return;
        }

        // we finished the current block, switch to the next
        // one.
        currentPositionIndex++;

        if (currentPositionIndex >= positionalHighlight.length)
            currentRealBeginning = huge_index; // really big value to avoid updating again.
        else {
            curr_pos = positionalHighlight[currentPositionIndex];
            currentRealBeginning = curr_pos.beg;
            currentEnd = curr_pos.end;
        }
    };

    /**
     * @type {function(number,number) : number}
     * return value <= 0 if no split is needed,
     *         value > n otherwise.
     */
    var index_splitter = function(index, size) {
        //                  hhhhhhhhhhhhhhh
        //      tttttt
        if (index + size < currentRealBeginning)
            return 0;

        var splitIndex = Math.min(index + size, currentEnd);

        use_to_position( splitIndex );

        return splitIndex;
    };

    /**
     * @type {function(function(string, string), string)}
     */
    var split_produce = function(producer, str) {
        var size = str.length;
        var curr_idx = previousIndex;
        var split = 1;

        while (size > 0)
        {
            var beg_distance = Math.min(size, Math.max(0, currentRealBeginning - curr_idx));

            if (curr_idx < currentRealBeginning && beg_distance > 0) {
                var beg = curr_idx - previousIndex;
                producer('', str.slice(beg, beg + beg_distance));
                curr_idx += beg_distance;
                size -= beg_distance;
            }

            if (size <= 0) break;

            split = index_splitter(curr_idx, size);

            if (split > 0) {
                var sub = str.slice(curr_idx - previousIndex, split - previousIndex);
                producer('sub', sub);
                size -= split - curr_idx;
                curr_idx = split;
            }
            
        } 

        previousIndex += str.length;
    };

    /** @type {function(!Array.<subrange>)} */
    var setter = function(lst) {
        positionalHighlight  = lst;
    }

    return {
        positional_setter: setter,
        reset: inner_reset,
        split_parts: split_produce
    };
};

/** @typedef ({kind: string, recognizer: function(string, number) : string}) */
var SyntaxParser;


/** @typedef ({regions: Array.<LangDef>}) */
var LangDef;

/** @typedef ({  colorLine: function(string) : Array.<Element>,
 *        with_line_number: boolean,
 *    setPositionHighlight: function(Array.<subrange>),
 *     compute_line_number: function(),
 * set_current_line_number: function(number),
 *                    lang: LangDef}) */
var LineHighlighter;

/** @typedef (function (string, number) : number) */
var RegionParser;

/** @module */
var TinySyntaxHighlighter = (function () {
    "use strict";

    /** @type {SubHighlighter} */
    var pos_highlight = new PositionnalHighlighter();

    /** @type {function(string, string, number) : boolean} */
    function isTokenPrefixOf( pref, line, base ) {
        if (line.length < pref.length || pref.length === 0) return false;

        for ( var i = 0; i < pref.length; i++ )
        {
            if (pref[i] !== line[i + base])
                return false;
        }

        return true;
    }

    /** @type {function(string, number) : number} */
    var nextSpaceIndex = function ( line, idx ) {
        var spaceIndex = line.indexOf(' ', idx );
        var tabIndex = line.indexOf('\t', idx );
        return Math.min( spaceIndex, tabIndex );
    }

    /** @type {function(string) : string} */
    var html_encodize = function(snipp) {
        return snipp.replace(/\&/g, '\&amp;').replace(/</g, '\&lt;').replace(/</g, '\&gt;');
    }

    /** @type {function(classname, string) : Element} */
    var context_free_highlight = function(kind, txt) {
        var span = document.createElement('span');
        var txtNode = document.createTextNode(txt);
        span.setAttribute('class', kind);
        span.appendChild(txtNode);
        return span
    }

    /** @type {function(classname, string) : Array.<Element>} */
    var highlight = function(kind, txt) {
        /** @type {Array.<Element>} */
        var ret = [];

        pos_highlight.split_parts(function(k, sub_str) {
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

    /** @type {function(string) : Array.<Element>} */
    var writeSubSplittedText = function( str ) {
        /** @type {Array.<Element>} */
        var ret = [];

        pos_highlight.split_parts(function( k, sub_str ) {
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

    /** @type {function(this:LineHighlighter, string) : Array.<Element>} */
    var colorLine = function ( line ) {
        /** @type {number} */
        var maxIndex = line.length;

        /** @type {boolean} */
        var consumed = false;

        /** @type {Array.<Element>} */
        var ret = [];

        /** @type {string} */
        var textAccumulator = '';

        pos_highlight.reset();

        /** @type {function(string, string)} */
        var globNode = function(kind, tok) {
            /** @type {Element} */
            var span = document.createElement('span');
            var maxi = ret.length;

            if (tok !== '') addText(tok);

            span.setAttribute('class', kind);

            for ( var i = 0; i < maxi; i++ )
                { span.appendChild(ret[i]); }

            ret = [span];
        };

        /** @type {function()} */
        var flushText = function() {
            if (textAccumulator === '') return;
            var rez = writeSubSplittedText( textAccumulator );

            for (var i = 0; i < rez.length; i++)
                ret.push(rez[i]);

            textAccumulator = '';
        };

        /** @type {function(classname, string)} */
        var line_hi = function(kind, txt) {
            flushText();
            return highlight(kind, txt);
        };

        /** @type {function(!Array.<Element>)} */
        var addNode = function(nodes) {
            for ( var i = 0; i < nodes.length; i++ )
                ret.push(nodes[i]);
        };

        /** @type {function(!string)} txt */
        var addText = function(txt) {
            textAccumulator += txt;
        }

        /** @type {number} */
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
                    addText(line.slice(currentIndex, currentIndex + consumed_chars));
                    consumed = true;
                    currentIndex += consumed_chars;
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
        for ( var i = this.activeStack.length - 1; i > 0; i-- ) {
            globNode(this.activeStack[i].kind, '');
        }

        var num = this.compute_line_number();
        if (num !== undefined)
            ret.unshift(num);
        return ret;
    };

    /** @type {function(string) : Array.<Element>} */
    var basic_highlighter = function(line) {
        pos_highlight.reset();

        var ret = writeSubSplittedText( line );
        var num = this.compute_line_number();
        if (num !== undefined)
            ret.unshift(num);

        return ret;
    }

    /** Create a highlighter which just pass-through the line
     * @type {function(new:LineHighlighter, boolean)}
     */
    var create_empty_highlighter = function(with_line_number) {
        this.colorLine = basic_highlighter;
        this.with_line_number = with_line_number;
        this.setPositionHighlight = function(lst) {
            pos_highlight.positional_setter(lst);
        };

        this.current_line = 1;
        this.set_current_line_number = function (i) {
            this.current_line = i;
        };

        this.compute_line_number = function () {
          this.current_line = this.current_line + 1;
          if (this.with_line_number) {
            return context_free_highlight('syntax_line_number', (this.current_line - 1).toString());
          }
          return undefined;
        };

        return this;
    };

    /** Create a highlighter for a give language
     * @type {function(new:LineHighlighter, boolean, LangDef)}
     */
    var create_highlighter = function( with_line_number, highlight_def ) {
        create_empty_highlighter.call( this, with_line_number );
        this.setPositionHighlight = function(lst) {
            pos_highlight.positional_setter(lst);
        };
        this.activeStack = [highlight_def];
        this.colorLine = colorLine;

        /** @type {LangDef} */
        this.lang = highlight_def;
        return this;
    };

    var change_parser_kind = function(k, p) {
        return { kind: k, recognizer: p.recognizer };
    };

    /** @type {function(string, SyntaxParser) : (function(string, number) : string)} */
    var prefix_parser_kind = function(prefix, f) {
        return function( line, idx ) {
            if (line[idx] !== prefix)
                return '';
            var sub = f.recognizer(line, idx + 1);
            if (sub === '') return '';
            return prefix + sub;
        };
    }

    var exact_token_match = function(k, txt) {
        return { kind: k, recognizer: function(line, idx) {
                if (isTokenPrefixOf(txt, line, idx))
                    return txt;
                return '';
            }
        };
    }

    /** @type {Object.<string, SyntaxParser>} */
    var generic_parsers = {
        integer:
            { kind: 'syntax_number'
            , recognizer: 
                    /** @type {function( string, number ) : string} */
                    function( line, idx ) {
                    /** @type {number} */
                    var currIdx = idx;
                    while (currIdx < line.length && line[currIdx].match(/[0-9]/))
                        { currIdx++; }

                    return line.substring(idx, currIdx);
              }
            },

        c_like_identifier:
            { kind: ''
            , recognizer: 
                    /** @type {function( string, number ) : string} */
                    function( line, idx ) {
                /** @type {number} */
                var currIdx = idx;

                if (currIdx < line.length && !line[currIdx++].match(/[a-zA-Z]/))
                    return '';

                while (currIdx < line.length && line[currIdx].match(/[_a-zA-Z0-9]/))
                    { currIdx++; }

                return line.substring(idx, currIdx);
              }
            },

        c_like_preproc:
            { kind: 'syntax_preproc'
            , recognizer: 
                    /** @type {function( string, number ) : string} */
                    function( line, idx ) {
                    if (line[idx] !== '#') return '';

                    /** @type {number} */
                    var currIdx = idx + 1;

                    while (currIdx < line.length && line[currIdx].match(/ \t/))
                        currIdx++;

                    if (currIdx >= line.length) return '';

                    while (currIdx < line.length && line[currIdx].match(/[a-zA-Z0-9_]/))
                        currIdx++;

                    return line.substring(idx, currIdx);
                }
            },

        simple_quote_string:
            { kind: 'syntax_string'
            , recognizer:
                /** @type {function( string, number ) : string} */
                function( line, idx ) {
                if (line[idx] !== "'") return '';

                /** @type {number} */
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
            },

        double_quote_string:
            { kind: 'syntax_string'
            , recognizer: function( line, idx ) {
                if (line[idx] !== '"') return '';

                /** @type {number} */
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
            },

        back_quote_string:
            { kind: 'syntax_special'
            , recognizer: function( line, idx ) {
                if (line[idx] !== '`') return '';

                /** @type {number} */
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
            },

        monoline_comment: function( commentPrefix ) {
            return { kind:'syntax_comment'
                   , recognizer: function( line, idx ) {
                        return (isTokenPrefixOf( commentPrefix, line, idx ) 
                               ? line.substring(idx, line.length)
                               : '');
                   }
                }
        }
    }

    function expand_keyword_groups( lst ) {
        var ret = {};

        for ( var group in lst )
        {
            var g = lst[group];
            for ( var word in g.words )
                ret[g.words[word]] = g.kind;
        }

        return ret;
    }

    /** @type {RegionParser} */
    var null_region = function (line, base) { return 0; }

    /** @type {function(string) : RegionParser} */
    var tok_region = function(tok) {
        return function(line, base) {
            if ( isTokenPrefixOf(tok, line, base) )
                return tok.length;
            else
                return 0;
        }
    };

    /** @type {function(string, regexp) : SyntaxParser} */
    var rexp_parser = function( k, rexp ) {
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

    /** @const
     * @type {LangDef}
     */
    var rubyDef = {
        begin:null_region, end:null_region,

        parsers:[ generic_parsers.monoline_comment('#')
                , generic_parsers.double_quote_string
                , generic_parsers.simple_quote_string
                , generic_parsers.integer
                , generic_parsers.c_like_identifier
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

    /** @type {LangDef} */
    var xmlDef = {
        begin:null_region, end:null_region,
        keywords: [],
        parsers: [rexp_parser('syntax_special', /&[0-9A-Za-z]{1,8};/g)],
        regions:[{ begin:tok_region('<!--')
                 , end:tok_region('-->')
                 , kind:"syntax_comment"
                 , regions:[], parsers:[], keywords:[] }

                ,{ begin:function(l, currentIndex) {
                            if (l[currentIndex] !== '<') return 0;
                            if (currentIndex + 1>= l.length) return 0;
                            if (l[currentIndex + 1].match(/[a-zA-Z]/))
                                return 1;

                            return 0;
                         }
                 , end:tok_region(">")
                 , kind:"syntax_tag"
                 , regions:[]
                 , parsers:[ generic_parsers.double_quote_string
                           , generic_parsers.simple_quote_string
                           , change_parser_kind('syntax_tag_name', generic_parsers.c_like_identifier)
                           ]
                 , keywords:[]
                 }

                ,{ begin:function(l, currentIndex) {
                            if (l[currentIndex] !== '<') return 0;
                            if (currentIndex + 1>= l.length) return 0;
                            if (l[currentIndex + 1] !== '/') return 0;

                            return 2;
                         }

                 , end:tok_region(">")
                 , kind:"syntax_tag"
                 , regions:[]
                 , parsers:[ change_parser_kind('syntax_tag_name', generic_parsers.c_like_identifier) ]
                 , keywords:[]
                 }
                 ]
    };

    var cssDef = {
        begin:null_region, end:null_region,

        regions:[{ begin:tok_region("/*")
                 , end:tok_region("*/")
                 , kind:"syntax_comment"
                 , parsers:[], keywords:[] 
                 , regions:[]}

                ,{ begin:tok_region("{")
                 , end:tok_region("}")
                 , kind:""
                 , regions:[]
                 , parsers:
                    [ generic_parsers.double_quote_string
                    , rexp_parser( 'syntax_storage_class', /[a-zA-Z][a-zA-Z0-9-]*\s*(?=:)/ )
                    , rexp_parser( 'syntax_number',
                                   /[-+]?\d+(\.\d*)?(%|mm|cm|in|pt|pc|em|ex|px)/ )
                    , rexp_parser( 'syntax_constant', /#[0-9a-fA-F]{6}/ )
                    , rexp_parser( 'syntax_constant', /#[0-9a-fA-F]{3}/ )
                    ]
                 , keywords:[]
                 }
                ],

        parsers:[ { kind:'syntax_structure'
                  , recognizer:prefix_parser_kind('.', generic_parsers.c_like_identifier )}
                , { kind:'syntax_function'
                  , recognizer:prefix_parser_kind('#', generic_parsers.c_like_identifier )}
                , { kind:'syntax_preproc'
                  , recognizer:prefix_parser_kind(':', generic_parsers.c_like_identifier )}
                , generic_parsers.c_like_identifier
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



    /** @type {LangDef} */
    var shellDef = {
        begin:null_region, end:null_region,

        parsers:[ generic_parsers.monoline_comment('#')
                , generic_parsers.double_quote_string
                , generic_parsers.simple_quote_string
                , generic_parsers.back_quote_string
                , generic_parsers.integer
                , rexp_parser('syntax_identifier', /[a-zA-Z0-9.!@_%+,]*(?==)/)
                , rexp_parser('syntax_operator', /<<|>>|!=|==|\||\&>/)
                , exact_token_match('syntax_identifier', '$#')
                , { kind: 'syntax_identifier'
                  , recognizer:prefix_parser_kind('$', generic_parsers.c_like_identifier )}
                , generic_parsers.c_like_identifier
                ],
            
        keywords: expand_keyword_groups(
            [ { kind:'syntax_conditional', words: ["if", "else", "elif", "fi", "then", "case", "esac"] }
            , { kind:'syntax_repeat'     , words: ["while", "until", "do", "for", "done", "in"] }
            , { kind:'syntax_function'   , words: ["function"] }
            ]),
        
        regions:[]
    };

    /** @type {LangDef} */
    var pythonDef = {
        begin:null_region, end:null_region,

        parsers:[ generic_parsers.monoline_comment('#')
                , generic_parsers.double_quote_string
                , generic_parsers.simple_quote_string
                , generic_parsers.integer
                , generic_parsers.c_like_identifier
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

        regions:[]
    };

    /** @type {LangDef} */
    var makefileDef = {
        begin:null_region, end:null_region,

        parsers:[ generic_parsers.monoline_comment('#')
                 // target
                , rexp_parser('syntax_function', /^[A-Za-z0-9_./$()%-][A-Za-z0-9_./\t $()%-]*::?/)
                , generic_parsers.c_like_identifier
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

    function make_region_recursive(region) {
        region.regions.push(region);
        return region;
    }

    /** @type {LangDef} */
    var ocamlDef = {
        begin:null_region, end:null_region,

        regions: [make_region_recursive({ begin:tok_region("(*")
                                        , end:tok_region("*)"), kind:"syntax_comment"
                                        , regions:[], parsers:[], keywords:[] })],

        parsers:[ generic_parsers.c_like_identifier
                , generic_parsers.double_quote_string
                , generic_parsers.integer
                , rexp_parser('syntax_constant', /[A-Z][A-Za-z0-9_]+/)
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
                                             ,'done', 'open'
                                             ,'try', 'type', 'virtual', 'when', 'while', 'with'] }
            , { kind:'syntax_typedef', words: ['type'] }
            , { kind:'syntax_bool'     , words:["True", "False"] }
            , { kind:'syntax_operator', words:['asr', 'lnot', 'lor', 'lsl', 'lsr'
                                              ,'lxor', 'mod', 'not'] }

            , { kind:'syntax_type'
              , words:['array', 'bool', 'char', 'exn', 'float',
                       'format', 'format4', 'int', 'int32', 'int64',
                       'lazy_t', 'list', 'nativeint', 'option',
                       'string', 'unit']}

            ])
    };
    /** @type {LangDef} */
    var haskellDef = {
        begin:null_region, end:null_region,

        regions: [make_region_recursive({ begin:tok_region("{-")
                                        , end:tok_region("-}"), kind:"syntax_comment"
                                        , regions:[], parsers:[], keywords:[] })],

        parsers:[ generic_parsers.c_like_identifier
                , generic_parsers.monoline_comment('--')
                , generic_parsers.double_quote_string
                , generic_parsers.integer
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

    /** @type {LangDef} */
    var javascriptDef = {
        begin:null_region, end:null_region,

        regions:[{ begin:tok_region("/*")
                 , end:tok_region("*/")
                 , kind:"syntax_comment"
                 , regions:[], parsers:[], keywords:[] }],

        parsers:[ generic_parsers.c_like_identifier
                , generic_parsers.double_quote_string
                , generic_parsers.simple_quote_string
                , generic_parsers.integer
                , generic_parsers.monoline_comment('//')
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

    /** @type {LangDef} */
    var phpDef = {
        begin:null_region, end:null_region,

        regions:[{ begin:tok_region("/*")
                 , end:tok_region("*/")
                 , kind:"syntax_comment"
                 , regions:[], parsers:[], keywords:[] }],
        
        parsers:[ { kind:'syntax_identifier'
                  , recognizer:prefix_parser_kind('$', generic_parsers.c_like_identifier )}
                , generic_parsers.double_quote_string
                , generic_parsers.integer
                , generic_parsers.monoline_comment('//')
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

    /** @type {LangDef} */
    var cDef = {
        begin:null_region, end:null_region,

        regions:[{ begin:tok_region("/*")
                 , end:tok_region("*/")
                 , kind:"syntax_comment"
                 , regions:[], parsers:[], keywords:[] }],

        parsers:[ generic_parsers.c_like_identifier
                , generic_parsers.c_like_preproc
                , generic_parsers.double_quote_string
                , generic_parsers.integer
                , generic_parsers.monoline_comment('//')
                ],
            
        keywords:expand_keyword_groups(
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
            ])
    };

    /** @const
     * @type {LangDef}
     */
    var cppDef = (function() {
        var cppOnlyDef = {
            keywords:expand_keyword_groups(
                [ { kind:'syntax_statement', words:['new', 'delete', 'this', 'friend', 'using'] }
                , { kind:'syntax_statement', words:["public", "protected", "private"] }
                , { kind:'syntax_bool'     , words:["true", "false"] }
                , { kind:'syntax_type', words:["inline", "virtual", "explicit", "export", "bool", "wchar_t"] }
                , { kind:'syntax_exception', words:["throw", "try", "catch"] }
                , { kind:'syntax_operator', words:["operator", "typeid", "and", "bitor", "or", "xor"
                                                  ,"compl" ,"bitand", "and_eq", "or_eq", "xor_eq", "not"
                                                  , "not_eq"]}
                , { kind:'syntax_structure', words:["class", "typename", "template", "namespace"] }
                ]
            )
        };

        $.extend(true, cppOnlyDef, cDef);

        return cppOnlyDef;
    })();

    var assoc = function(expr, lang) {
        return {pattern: expr, def: lang};
    };

    var langAssoc = [
        assoc(/\.rb$/      , rubyDef),
        assoc(/\.c$/       , cDef),
        assoc(/\.cpp$/     , cppDef),
        assoc(/\.cc$/      , cppDef),
        assoc(/\.h$/       , cppDef),
        assoc(/\.hpp$/     , cppDef),
        assoc(/\.php$/     , phpDef),
        assoc(/\.ml$/      , ocamlDef),
        assoc(/\.mli$/     , ocamlDef),
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
        assoc(/[mM]akefile$/, makefileDef)
    ];

    function instantiate_from_filename(with_line_number, filename)
    {
        var i;

        for (i = 0; i < langAssoc.length; i++)
        {
            if (filename.match(langAssoc[i].pattern))
                return new create_highlighter(with_line_number, langAssoc[i].def);
        }

        return new create_empty_highlighter(with_line_number);
    }

    return {
        c_highlighter: function (with_line_number)
            { return new create_highlighter( with_line_number, cDef ); },

        css_highlighter: function (with_line_number)
            { return new create_highlighter( with_line_number, cssDef ); },

        cpp_highlighter: function (with_line_number)
            { return new create_highlighter( with_line_number, cppDef ); },

        haskell_highlighter: function(with_line_number)
            { return new create_highlighter( with_line_number, haskellDef ); },

        ruby_highlighter: function(with_line_number)
            { return new create_highlighter( with_line_number, rubyDef ); },

        empty_highlighter: function(with_line_number)
            { return new create_empty_highlighter(with_line_number); },

        from_filename: instantiate_from_filename
    };
})();

