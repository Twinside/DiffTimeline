var TinySyntaxHighlighter = (function () {
    "use strict";

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

    var colorLine = function ( line ) {
        var maxIndex = line.length;
        var currentIndex = 0;
        var consumed = false;
        var ret = this.compute_line_number();

        for ( var j = 1; j < this.activeStack.length; j++ )
        {
            var e = this.activeStack[j];
            ret += '<span class="' + e.kind + '">';
        }

        while (currentIndex < maxIndex)
        {
            while (currentIndex < maxIndex &&
                   line[currentIndex] === ' ' || line[currentIndex] === '\t')
            {
                ret += line[currentIndex];
                currentIndex++;
            }

            if (currentIndex >= maxIndex) break;

            var current_region = this.activeStack[ this.activeStack.length - 1 ];

            if (isTokenPrefixOf(current_region.end, line, currentIndex))
            {
                if (this.activeStack.length > 1)
                    this.activeStack.pop();
                ret += current_region.end + '</span>';
                currentIndex += current_region.end.length;
                continue;
            }

            for ( var i in current_region.regions )
            {
                var r = current_region.regions[i];

                if (isTokenPrefixOf(r.begin, line, currentIndex))
                {
                    this.activeStack.push(r);
                    ret += '<span class="' + r.kind + '">' + r.begin;
                    consumed = true;
                    currentIndex += r.begin.length;
                    break;
                }
            }

            if (currentIndex >= maxIndex) break;

            for ( var i in current_region.parsers )
            {
                var parser = this.def.parsers[i];
                var parserRet = parser.recognizer(line, currentIndex);

                if (parserRet !== '')
                {
                    if (this.def.keywords.hasOwnProperty(parserRet))
                    {
                        var found_class = this.def.keywords[parserRet]
                        ret += '<span class="' + found_class + '">' + parserRet + '</span>';
                    }
                    else if (parser.kind !== '')
                    {
                        ret += '<span class="' + parser.kind + '">' + html_encodize(parserRet) + '</span>';
                    }
                    else ret += html_encodize(parserRet);

                    currentIndex += parserRet.length;
                    consumed = true;
                    break;
                }
            }

            if (!consumed)
                { ret += html_encodize(line[currentIndex++]); }

            consumed = false;
        }

        // end of line, we must close all the
        // active tags (have to be reoppened a
        // the beginning
        for ( var i = 1; i < this.activeStack.length; i++ )
            { ret += '</span>'; }

        return ret;
    };

    var basic_highlighter = function(line) {
        var ret = this.compute_line_number();
        return ret + html_encodize(line);
    }

    /** Create a highlighter which just pass-through the line
     * @constructor
     */
    var create_empty_highlighter = function(with_line_number) {
        this.colorLine = basic_highlighter;
        this.with_line_number = with_line_number;
        if (with_line_number) {
          this.current_line = 1;
          this.set_current_line_number = function (i) {
            this.current_line = i;
          };

          this.compute_line_number = function () {
            var ret = '';
            if (this.with_line_number) {
              this.current_line = this.current_line + 1;
              return '<span class="syntax_line_number">' + (this.current_line - 1)+ '</span>';
            }
            return '';
          };
        }
        else
        {
          this.set_current_line_number = function (i) {};
          this.compute_line_number = function () { return ''; };
        }
        return this;
    };

    /** Create a highlighter for a give language
     * @constructor
     */
    var create_highlighter = function( with_line_number, highlight_def ) {
        create_empty_highlighter.call( this, with_line_number );
        this.activeStack = [highlight_def];
        this.colorLine = colorLine;
        this.def = highlight_def;
        return this;
    };

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

    /** @const */
    var rubyDef = {
        begin:'', end:'',

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

    var pythonDef = {
        begin:'', end:'',

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

    var haskellDef = {
        begin:'', end:'',

        regions: [make_region_recursive({ begin:"{-", end:"-}", kind:"syntax_comment"
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

    var javascriptDef = {
        begin:'', end:'',

        regions:[{ begin:"/*", end:"*/", kind:"syntax_comment"
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

    /** @const */
    var cDef = {
        begin:'', end:'',

        regions:[{ begin:"/*", end:"*/", kind:"syntax_comment"
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

