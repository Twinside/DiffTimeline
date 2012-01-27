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
            while (line[currentIndex] === ' ' || line[currentIndex] === '\t')
            {
                ret += line[currentIndex];
                currentIndex++;
            }

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
                        ret += '<span class="' + this.def.keywords[parserRet];
                        ret += + '">' + parserRet + '</span>';
                    }
                    else ret += '<span class="' + parser.kind + '">' + parserRet + '</span>';

                    currentIndex += parserRet.length;
                    consumed = true;
                    break;
                }
            }

            if (!consumed)
                { ret += line[currentIndex++]; }

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

    var rubyDef = {
        parsers:[
            { recognizer: function( line, idx ) {
                    if (line[idx] !== '#') return '';
                    return line.substring(idx, line.length - 1);
              }
            , kind: 'syntax_comment'
            },

            { recognizer: function( line, idx ) {
                var currIdx = idx;

                if (currIdx < line.length && !line[currIdx++].match(/[a-zA-Z]/))
                    return '';

                while (currIdx < line.length && line[currIdx].match(/[_a-zA-Z0-9]/))
                    { currIdx++; }

                return line.substring(idx, currIdx);
              }

            , kind: 'syntax_identifier'
            }
        ],
            
        keywords:{
            "begin" :"syntax_conditional",
            "end"   :"syntax_conditional",
            "switch":"syntax_conditional",
            "else"  :"syntax_conditional",

            "for"    :"syntax_loop" ,
            "while"  :"syntax_loop" ,
            "do"     :"syntax_loop" 
        }
        
    }

    var cDef = {
        regions:[
            { begin:"/*", end:"*/", kind:"syntax_comment", nested:false },
            { begin:"//", end:"\n", kind:"syntax_comment", nested:false }
        ],

        parsers:[
            //{ recognizer: function( line, idx ) {
                    //if (line[idx] !== '"') return '';
              //}
            //, kind: 'syntax_string'
            //},

            { recognizer: function( line, idx ) {
                var currIdx = idx;

                if (currIdx < line.length && !line[currIdx++].match(/[a-zA-Z]/))
                    return '';

                while (currIdx < line.length && line[currIdx].match(/[_a-zA-Z0-9]/))
                    { currIdx++; }

                return line.substring(idx, currIdx);
              }

            , kind: 'syntax_identifier'
            }
        ],
            
        keywords:{
            "if"    :"syntax_conditional",
            "switch":"syntax_conditional",
            "else"  :"syntax_conditional",

            "for"    :"syntax_loop" ,
            "while"  :"syntax_loop" ,
            "do"     :"syntax_loop" 
        }
    };
    
    return {
        c_highlighter: function () { return new createHighlighter( rubyDef /*cDef */); }
    };
})();
