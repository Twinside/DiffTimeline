var TinySyntaxHighlighter = (function () {

    var findNextBeginIndex = function( line, idx ) {
        var returnIndex = 99999;
        
        var nextSpaceIndex = line.indexOf(' ', currentIndex);
        var nextTabIndex = line.indexOf('\t', currentIndex);
    };

    function isTokenPrefixOf( pref, line, base ) {
        if (line.length < pref.length) return false;

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
        var ret = "";
        var consumed = false;

        for ( var j in this.activeStack )
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

            for ( var i in this.def.regions )
            {
                var r = this.def.regions[i];
                if (isTokenPrefixOf(r.begin, line, currentIndex))
                {
                    this.activeStack.push(r.kind);
                    ret += '<span class="' + r.kind + '">';
                    consumed = true;
                    currentIndex += r.begin.length;
                    break;
                }
                else if (isTokenPrefixOf(r.end, line, currentIndex))
                {
                    this.activeStack.pop();
                    ret += '</span>';
                    currentIndex += r.end.length;
                    break;
                }
            }

            for ( var i in this.def.parsers )
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
                    else
                    {
                        ret += '<span class="' + parser.kind + '">' + html_encodize(parserRet) + '</span>';
                    }

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
        for ( var i = 0; i < this.activeStack.length; i++ )
            { ret += '</span>'; }

        return ret;
    };

    var createHighlighter = function( highlightDef ) {
        this.activeStack = [];
        this.def = highlightDef;
        this.colorLine = colorLine;
        return this;
    };
    var generic_parsers = {
        conf_comment:
            { kind: 'syntax_comment'
            , recognizer: function( line, idx ) {
                    if (line[idx] !== '#') return '';
                    return line.substring(idx, line.length - 1);
              }
            },

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
            { kind: 'syntax_identifier'
            , recognizer: function( line, idx ) {
                var currIdx = idx;

                if (currIdx < line.length && !line[currIdx++].match(/[a-zA-Z]/))
                    return '';

                while (currIdx < line.length && line[currIdx].match(/[_a-zA-Z0-9]/))
                    { currIdx++; }

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

    var rubyDef = {
        parsers:[ generic_parsers.conf_comment
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
            , { kind:'syntax_loop'       , words: ["while", "until", "for", "in"] }
            , { kind:'syntax_constant'   , words: ["nil", "self", "__FILE__", "__LINE__"] }
            ]),
        
        regions:[]
    }

    var cDef = {
        regions:[{ begin:"/*", end:"*/", kind:"syntax_comment", nested:false }],

        parsers:[generic_parsers.c_like_identifier],
            
        keywords:{
            'if'    :'syntax_conditional',
            'switch':'syntax_conditional',
            'else'  :'syntax_conditional',

            'for'    :'syntax_loop' ,
            'while'  :'syntax_loop' ,
            'do'     :'syntax_loop' 
        }
    };
    
    function instantiate_from_filename(filename)
    {
        if (filename.match(/\.rb$/))
            return new createHighlighter( rubyDef );
        else if (filename.match(/\.c$/))
            return new createHighlighter( cDef );

        return new createHighlighter( cDef );
    }

    return {
        c_highlighter: function () { return new createHighlighter( cDef ); },
        ruby_highlighter: function() { return new createHighlighter( rubyDef ); },

        from_filename: instantiate_from_filename
    };
})();
