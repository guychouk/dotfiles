set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif

let colors_name = "{{theme}}"

let s:palette = {
      \ 'ui':            '{{ui}}',
      \ 'pum':           '{{pum}}',
      \ 'black':         '{{black}}',
      \ 'brightblack':   '{{brightblack}}',
      \ 'red':           '{{red}}',
      \ 'brightred':     '{{brightred}}',
      \ 'green':         '{{green}}',
      \ 'brightgreen':   '{{brightgreen}}',
      \ 'orange':        '{{orange}}',
      \ 'yellow':        '{{yellow}}',
      \ 'blue':          '{{blue}}',
      \ 'brightblue':    '{{brightblue}}',
      \ 'magenta':       '{{magenta}}',
      \ 'plum':          '{{plum}}',
      \ 'brightmagenta': '{{brightmagenta}}',
      \ 'cyan':          '{{cyan}}',
      \ 'brightcyan':    '{{brightcyan}}',
      \ 'gray':          '{{gray}}',
      \ 'white':         '{{white}}',
      \ 'dimgray':       '{{dimgray}}',
      \ 'guide':         '{{guide}}',
      \ }

let g:terminal_ansi_colors = [
      \ s:palette['black'],
      \ s:palette['red'],
      \ s:palette['green'],
      \ s:palette['orange'],
      \ s:palette['blue'],
      \ s:palette['magenta'],
      \ s:palette['cyan'],
      \ s:palette['gray'],
      \ s:palette['brightblack'],
      \ s:palette['brightred'],
      \ s:palette['brightgreen'],
      \ s:palette['yellow'],
      \ s:palette['brightblue'],
      \ s:palette['brightmagenta'],
      \ s:palette['brightcyan'],
      \ s:palette['white'],
      \ ]

function! s:hl(group, fg, bg, attr) abort
  execute 'hi' a:group
        \ 'guifg=' . (a:fg ==# '' ? 'NONE' : s:palette[a:fg])
        \ 'guibg=' . (a:bg ==# '' ? 'NONE' : s:palette[a:bg])
        \ 'gui='   . (a:attr ==# '' ? 'NONE' : a:attr)
        \ 'cterm=' . (a:attr ==# '' ? 'NONE' : a:attr)
endfunction

call s:hl('StatusLine',        'white',         'ui',            '')
call s:hl('StatusLineNC',      'gray',          'ui',            '')
call s:hl('StatusLineTerm',    'white',         'ui',            '')
call s:hl('StatusLineTermNC',  'gray',          'ui',            '')
call s:hl('TabLine',           'white',         'ui',            '')
call s:hl('TabLineSel',        'ui',            'white',         '')
call s:hl('TabLineFill',       '',              'ui',            '')
call s:hl('Cursor',            '',              'yellow',        '')
call s:hl('CursorColumn',      '',              'gray',          '')
call s:hl('CursorLine',        '',              'gray',          '')
call s:hl('CursorLineNr',      'magenta',       'black',         'bold')
call s:hl('FoldColumn',        '',              '',              '')
call s:hl('Folded',            'gray',          '',              '')
call s:hl('LineNr',            'gray',          '',              '')
call s:hl('MatchParen',        'black',         'white',         'bold')
call s:hl('NonText',           'gray',          '',              '')
call s:hl('SpecialKey',        'guide',         '',              '')
call s:hl('Whitespace',        'guide',         '',              '')
call s:hl('Normal',            'white',         'black',         '')
call s:hl('Terminal',          'white',         'black',         '')
call s:hl('PmenuSel',          'brightblack',   'brightmagenta', '')
call s:hl('Pmenu',             'brightmagenta', 'pum',           '')
call s:hl('PmenuSbar',         'brightmagenta', 'pum',           '')
call s:hl('PmenuThumb',        'brightmagenta', 'brightmagenta', '')
call s:hl('PmenuBorder',       'magenta',       'black',         '')
call s:hl('QuickFixLine',      'brightmagenta', 'plum',          '')
call s:hl('SignColumn',        'black',         '',              '')
call s:hl('Title',             'white',         '',              'bold')
call s:hl('VertSplit',         'ui',            'ui',            '')
call s:hl('Visual',            'yellow',        'ui',            '')
call s:hl('qfFileName',        'green',         '',              '')
call s:hl('qfLineNr',          'gray',          '',              '')
call s:hl('ErrorMsg',          'red',           '',              '')
call s:hl('SpellBad',          'red',           '',              '')
call s:hl('SpellCap',          'blue',          '',              '')
call s:hl('SpellLocal',        'magenta',       '',              '')
call s:hl('SpellRare',         'magenta',       '',              '')
call s:hl('DiffAdd',           'green',         '',              '')
call s:hl('DiffChange',        'yellow',        '',              '')
call s:hl('DiffDelete',        'red',           '',              '')
call s:hl('diffAdded',         'green',         '',              '')
call s:hl('diffRemoved',       'red',           '',              '')
call s:hl('DiffChange',        'yellow',        '',              '')
call s:hl('DiffDelete',        'red',           '',              '')
call s:hl('DiffText',          '',              'ui',            '')
call s:hl('Boolean',           'magenta',       '',              '')
call s:hl('Comment',           'gray',          '',              'italic')
call s:hl('SpecialComment',    'gray',          '',              'italic')
call s:hl('Constant',          'magenta',       '',              '')
call s:hl('Function',          'white',         '',              '')
call s:hl('Identifier',        'magenta',       '',              '')
call s:hl('Keyword',           'magenta',       '',              '')
call s:hl('Number',            'green',         '',              '')
call s:hl('PreProc',           'white',        '',              '')
call s:hl('PreCondit',         'orange',        '',              '')
call s:hl('Include',           'magenta',       '',              '')
call s:hl('Macro',             'magenta',       '',              '')
call s:hl('Special',           'magenta',       '',              '')
call s:hl('Statement',         'magenta',       '',              '')
call s:hl('String',            'green',         '',              '')
call s:hl('Todo',              'magenta',       '',              '')
call s:hl('Type',              'magenta',       '',              '')
call s:hl('FzfBorder',         'magenta',       '',              '')
call s:hl('DirvishPathTail',   'green',         '',              'bold')
call s:hl('PicolineNRM',       'white',         'ui',            '')
call s:hl('PicolineINS',       'magenta',       'ui',            '')
call s:hl('PicolineVIS',       'magenta',       'ui',            '')
call s:hl('PicolineCMD',       'magenta',       'ui',            '')
call s:hl('PicolineRPL',       'magenta',       'ui',            '')
call s:hl('PicolineSEL',       'magenta',       'ui',            '')
call s:hl('PicolineTRM',       'magenta',       'ui',            '')
call s:hl('PicolinePRO',       'magenta',       'ui',            '')
call s:hl('PicolineEXT',       'magenta',       'ui',            '')
call s:hl('PicolinePEN',       'magenta',       'ui',            '')
call s:hl('PicolineSCH',       'magenta',       'ui',            '')
call s:hl('PicolineSeparator', 'dimgray',       'ui',            '')
