var TinySyntaxHighlighter = (function () {

    var findNextBeginIndex = function( line, idx ) {
        var returnIndex = 99999;
        
        var nextSpaceIndex = line.indexOf(' ', currentIndex);
        var nextTabIndex = line.indexOf('\t', currentIndex);
    };

    var isPrefixOf = function( pref, line, base ) {
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

        for ( var j in this.activeStack )
        {
            var e = this.activeStack[j];
            ret += '<span class="' + e.kind + '">';
        }

        while (currentIndex < maxIndex)
        {
            while (line[currentIndex] === ' ' || line[curentIndex] === '\t')
            {
                ret += line[currentIndex];
                currentIndex++;
            }

            for ( var i in this.def.regions )
            {
                var r = this.def.regions[i];
                if (isPrefixOf(r.begin, line, currentIndex))
                {
                    this.activeStack.push(r.kind);
                    ret += '<span class="' + r.kind + '">';
                    consumed = true;
                    currentIndex += r.begin.length;
                }
                else if (isPrefixOf(r.end, line, currentIndex))
                {
                    this.activeStack.pop();
                    ret += '</span>';
                    currentIndex += r.end.length;
                }
            }


            if (consumed) continue;
            var tokenIndex = nextSpaceIndex( line, currentIndex );
            var substr = line.substring(currentIndex, tokenIndex - 1);

            if (this.def.keywords.hasOwnProperty(substr))
            {
                ret += '<span class="' + this.def.keywords[substr];
                ret += substr + '</span>';
            }
            else ret += substr;

            currentIndex += substr.length;
        }

        // end of line, we must close all the
        // active tags (have to be reoppened a
        // the beginning
        for ( var i = 0; i < this.activeStack.length; i++ )
            { ret += '</span>'; }

        return ret;
    };

    var createHighlighter( highlightDef ) = function {
        this.activeStack = [];
        this.def = highlightDef;
        this.colorLine = colorLine;
        return this;
    };

    var cDef = {
        regions:[
            { begin:"/*", end:"*/", kind="syntax_comment", nested=false },
            { begin:"//", end:"\n", kind="syntax_comment", nested=false }
        ],

        parsers:[
            function( line, idx ) {
                if (line[idx] !== '"') return "";
            },

            function( line, idx ) {
                var currIdx = idx;
                if (!line[idx].match(/[a-zA-Z]/))
                    return "";

                idx++;
                while (line[currIdx].match(/[a-zA-Z]/))
                    { currIdx++; }

                return line.substring(idx, currIdx);
            }
        ]
            
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
        c_highlighter: function () { return createHighlighter( cDef ); }
    };
})();
