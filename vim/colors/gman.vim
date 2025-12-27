set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif

let colors_name = "gman"

let s:palette = {
      \ 'ui':            '#2a2c2c',
      \ 'black':         '#151515',
      \ 'brightblack':   '#555555',
      \ 'red':           '#fb413a',
      \ 'brightred':     '#febab7',
      \ 'green':         '#86df8d',
      \ 'brightgreen':   '#ebfaec',
      \ 'orange':        '#ff8c00',
      \ 'yellow':        '#fbfa7a',
      \ 'blue':          '#45566e',
      \ 'brightblue':    '#8295b1',
      \ 'magenta':       '#e67eb3',
      \ 'brightmagenta': '#fbe9f2',
      \ 'cyan':          '#e67eb3',
      \ 'brightcyan':    '#fbe9f2',
      \ 'gray':          '#808080',
      \ 'white':         '#f5f1e3',
      \ }

function! s:HL(group, fg, bg, attr)
  execute 'hi' a:group
        \ 'guifg=' . (a:fg ==# '' ? 'NONE' : s:palette[a:fg])
        \ 'guibg=' . (a:bg ==# '' ? 'NONE' : s:palette[a:bg])
        \ 'gui='   . (a:attr ==# '' ? 'NONE' : a:attr)
        \ 'cterm=' . (a:attr ==# '' ? 'NONE' : a:attr)
endfunction

call s:HL('StatusLine',      'white',   'ui',      '')
call s:HL('StatusLineNC',    'gray',    'ui',      '')
call s:HL('TabLine',         'white',   'ui',      '')
call s:HL('TabLineSel',      'ui',      'white',   '')
call s:HL('TabLineFill',     '',        'ui',      '')
call s:HL('Cursor',          '',        'yellow',  '')
call s:HL('CursorColumn',    '',        'gray',    '')
call s:HL('CursorLine',      '',        'gray',    '')
call s:HL('CursorLineNr',    'magenta', 'black',   'bold')
call s:HL('FoldColumn',      '',        '',        '')
call s:HL('Folded',          'gray',    '',        '')
call s:HL('LineNr',          'gray',    '',        '')
call s:HL('MatchParen',      'black',   'white',   'bold')
call s:HL('NonText',         'gray',    '',        '')
call s:HL('SpecialKey',      'gray',    '',        '')
call s:HL('Normal',          'white',   'black',   '')
call s:HL('Pmenu',           'white',   'ui',      '')
call s:HL('PmenuSel',        'black',   'magenta', '')
call s:HL('PmenuThumb',      'black',   'magenta', '')
call s:HL('QuickFixLine',    'ui',      'magenta', '')
call s:HL('SignColumn',      'black',   '',        '')
call s:HL('Title',           'white',   '',        'bold')
call s:HL('VertSplit',       'ui',      'ui',      '')
call s:HL('Visual',          'yellow',  'ui',      '')
call s:HL('qfFileName',      'green',   '',        '')
call s:HL('qfLineNr',        'gray',    '',        '')
call s:HL('ErrorMsg',        'red',     '',        '')
call s:HL('SpellBad',        'red',     '',        '')
call s:HL('SpellCap',        'blue',    '',        '')
call s:HL('SpellLocal',      'magenta', '',        '')
call s:HL('SpellRare',       'magenta', '',        '')
call s:HL('DiffAdd',         'green',   '',        '')
call s:HL('DiffChange',      'yellow',  '',        '')
call s:HL('DiffDelete',      'red',     '',        '')
call s:HL('diffAdded',       'green',   '',        '')
call s:HL('diffRemoved',     'red',     '',        '')
call s:HL('DiffChange',      'yellow',  '',        '')
call s:HL('DiffDelete',      'red',     '',        '')
call s:HL('DiffText',        '',        'ui',      '')
call s:HL('Boolean',         'magenta', '',        '')
call s:HL('Comment',         'gray',    '',        'italic')
call s:HL('Constant',        'magenta', '',        '')
call s:HL('Function',        'white',   '',        '')
call s:HL('Identifier',      'magenta', '',        '')
call s:HL('Keyword',         'magenta', '',        '')
call s:HL('Number',          'green',   '',        '')
call s:HL('PreProc',         'yellow',  '',        '')
call s:HL('Special',         'magenta', '',        '')
call s:HL('Statement',       'magenta', '',        '')
call s:HL('String',          'green',   '',        '')
call s:HL('Todo',            'magenta', '',        '')
call s:HL('Type',            'magenta', '',        '')
call s:HL('FzfBorder',       'magenta', '',        '')
call s:HL('DirvishPathTail', 'blue',    '',        'bold')
call s:HL('PicolineNRM',     'white',   'ui',      '')
call s:HL('PicolineINS',     'magenta', 'ui',      '')
call s:HL('PicolineVIS',     'magenta', 'ui',      '')
call s:HL('PicolineCMD',     'magenta', 'ui',      '')
call s:HL('PicolineRPL',     'magenta', 'ui',      '')
call s:HL('PicolineSEL',     'magenta', 'ui',      '')
call s:HL('PicolineTRM',     'magenta', 'ui',      '')
call s:HL('PicolinePRO',     'magenta', 'ui',      '')
call s:HL('PicolineEXT',     'magenta', 'ui',      '')
call s:HL('PicolinePEN',     'magenta', 'ui',      '')
call s:HL('PicolineSCH',     'magenta', 'ui',      '')

" Set ANSI colors 0-15 for vim's :terminal
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

" vim: sw=2 ts=2 et
