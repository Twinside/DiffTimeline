
let s:definitions =
    \ [ ["normal"      , 'normal'      , 'fg#']
	\ , ["comment"     , 'comment'     , 'fg#']
    \ , ["repeat"      , 'Repeat'      , 'fg#']
    \ , ["identifier"  , 'Identifier'  , 'fg#']
    \ , ["conditional" , 'Conditional' , 'fg#']
    \ , ["keyword"     , 'keyword'     , 'fg#']
    \ , ["number"      , 'Number'      , 'fg#']
    \ , ["statement"   , 'Statement'   , 'fg#']
    \ , ["preproc"     , 'Preproc'     , 'fg#']
    \ , ["bool"        , 'Boolean'     , 'fg#']
    \ , ["constant"    , 'Constant'    , 'fg#']
    \ , ["string"      , 'string'      , 'fg#']
    \ , ["storage_class", 'StorageClass', 'fg#']
    \ , ["structure"   , 'Structure'   , 'fg#']
    \ , ["label"       , 'Label'       , 'fg#']
    \ , ["operator"    , 'Operator'    , 'fg#']
    \ , ["tag"         , 'htmlTag'   , 'fg#']
    \ , ["tag_name"    , 'htmlTagName'     , 'fg#']
	\ , ["tag_param"   , 'htmlArg', 'fg#'  ]
	\ , ['special'	   , 'Special', 'fg#'  ]
    \ , ["function"    , 'Function'    , 'fg#']
    \ ]

fun! ConvertThemeToCss() "{{{
	let rez = []

	let normalfg = synIDattr(synIDtrans(hlID('Normal')), 'fg#', 'gui')
	let normalbg = synIDattr(synIDtrans(hlID('Normal')), 'bg#', 'gui')
	let lineNrfg = synIDattr(synIDtrans(hlID('LineNr')), 'fg#', 'gui')
	let lineNrbg = synIDattr(synIDtrans(hlID('LineNr')), 'bg#', 'gui')

	call add(rez, ".syntax_highlighted { color: " . normalfg . "; background: " . normalbg . "; }")
	call add(rez, ".syntax_line_number { color: " . lineNrfg . "; background: " . lineNrbg . "; }")
	
	for [name, vimAttr, info] in s:definitions
        let guiColor = synIDattr(synIDtrans(hlID(vimAttr)), info, 'gui')

		call add(rez, '.syntax_' . name . ' { color:' . guiColor . '; }')
	endfor

	call append(line('$'), rez)
endfunction "}}}

call ConvertThemeToCss()

