// meh, there is no real type for that
/** @typedef (Object) */
var regexp;

/** @typedef ({beg:number, end:number}) */
var subrange;

/** @typedef ({positional_setter:function(Array.<subrange>),
 *                         reset:function(),
 *                   split_parts:function(function(string, string), string)}) */
var SubHighlighter;

/**
 * @type {function(new:SubHighlighter)}
 */
var PositionnalHighlighter = function() {
    "use strict";

    /* Array storing information about diff at the line
     * level (inner subdiff), Should be used by all node
     * creating function to interleave proper syntax
     * highlighting and inner diff.
     * @type {Array.<subrange>}
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

    /* Update the state of the sub line highlighter,
     * update indices and detect end of boundary.
     * @type {function(number)}
     */
    var use_to_position = function( pos ) {
        var curr_pos = positionalHighlight[currentPositionIndex];
        if (pos < curr_pos.end) {
            currentRealBeginning = pos + 1;
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

        if (curr_idx < currentRealBeginning && currentRealBeginning <= str.length) {
            producer('', str.slice(curr_idx, currentRealBeginning));
            curr_idx = currentRealBeginning;
        }

        var split = index_splitter(curr_idx, size);

        while (split > 0) {
            producer('sub', str.slice(curr_idx - previousIndex, split - previousIndex));
            curr_idx = split;
            split = index_splitter(curr_idx, size);
        }

        if (curr_idx < previousIndex + size)
            producer('', str.slice(curr_idx - previousIndex, size));

        previousIndex += str.length;
    };

    /** @type {function(Array.<subrange>)} */
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

    function isTokenPrefixOf( pref, line, base ) {
        if (line.length < pref.length || pref.length === 0) return false;

        for ( var i = 0; i < pref.length; i++ )
        {
            if (pref[i] !== line[i + base])
                return false;
        }

        return true;
    }

    var nextSpaceIndex = function ( line, idx ) {
        var spaceIndex = line.indexOf(' ', idx );
        var tabIndex = line.indexOf('\t', idx );
        return Math.min( spaceIndex, tabIndex );
    }

    var html_encodize = function(snipp) {
        return snipp.replace(/\&/g, '\&amp;').replace(/</g, '\&lt;').replace(/</g, '\&gt;');
    }

    var context_free_highlight = function(kind, txt) {
        var span = document.createElement('span');
        var txtNode = document.createTextNode(txt);
        span.setAttribute('class', kind);
        span.appendChild(txtNode);
        return span
    }

    var highlight = function(kind, txt) {
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

    var writeSubSplittedText = function( str ) {
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
        var maxIndex = line.length;
        var currentIndex = 0;
        var consumed = false;
        var ret = [];
        var textAccumulator = "";

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

        var flushText = function() {
            if (textAccumulator === '') return;
            var rez = writeSubSplittedText( textAccumulator );

            for (var i = 0; i < rez.length; i++)
                ret.push(rez[i]);

            textAccumulator = '';
        };

        var line_hi = function(kind, txt) {
            flushText();
            return highlight(kind, txt);
        };

        var addNode = function(nodes) {
            for ( var i = 0; i < nodes.length; i++ )
                ret.push(nodes[i]);
        };

        var addText = function(txt) {
            textAccumulator += txt;
        }

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

        ret.unshift(this.compute_line_number());
        return ret;
    };

    var basic_highlighter = function(line) {
        pos_highlight.reset();

        var ret = writeSubSplittedText( line );
        ret.unshift(this.compute_line_number());

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

        if (with_line_number) {
          this.current_line = 1;
          this.set_current_line_number = function (i) {
            this.current_line = i;
          };

          this.compute_line_number = function () {
            var ret = '';
            if (this.with_line_number) {
              this.current_line = this.current_line + 1;
              return context_free_highlight('syntax_line_number', this.current_line - 1);
            }
            return undefined;
          };
        }
        else
        {
          this.set_current_line_number = function (i) {};
          this.compute_line_number = function () { return undefined; };
        }
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

    /** @type {Object.<string, SyntaxParser>} */
    var generic_parsers = {
        integer:
            { kind: 'syntax_number'
            , recognizer: function( line, idx ) {
                    var currIdx = idx;
                    while (currIdx < line.length && line[currIdx].match(/[0-9]/))
                        { currIdx++; }

                    return line.substring(idx, currIdx);
              }
            },

        c_like_identifier:
            { kind: ''
            , recognizer: function( line, idx ) {
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
            },

        simple_quote_string:
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
            },

        double_quote_string:
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

    function make_region_recursive(region) {
        region.regions.push(region);
        return region;
    }

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
            , { kind:'syntax_module', words:['module'] }
            , { kind:'syntax_preproc', words:['import'] }
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

    /** @const */
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
    
    function instantiate_from_filename(with_line_number, filename)
    {
        if (filename.match(/\.rb$/))
            return new create_highlighter(with_line_number, rubyDef );
        else if (filename.match(/\.c$/))
            return new create_highlighter(with_line_number, cDef );
        else if (filename.match(/\.cpp$/) || filename.match(/\.cc$/) ||
                 filename.match(/\.h$/)   || filename.match(/\.hpp$/))
            return new create_highlighter(with_line_number, cppDef );
        else if (filename.match(/\.hs$/))
            return new create_highlighter(with_line_number, haskellDef );
        else if (filename.match(/\.py$/))
            return new create_highlighter(with_line_number, pythonDef );
        else if (filename.match(/\.js$/))
            return new create_highlighter(with_line_number, javascriptDef );
        else if (filename.match(/\.xml$/) || filename.match(/\.html$/) || filename.match(/\.htm/))
            return new create_highlighter(with_line_number, xmlDef );

        return new create_empty_highlighter(with_line_number);
    }

    return {
        c_highlighter: function (with_line_number)
            { return new create_highlighter( with_line_number, cDef ); },

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

