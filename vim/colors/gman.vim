set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif

let colors_name = "gman"

let s:palette = {
      \ 'black':         '#151515',
      \ 'red':           '#fb413a',
      \ 'green':         '#86df8d',
      \ 'yellow':        '#fbfa7a',
      \ 'blue':          '#45566e',
      \ 'magenta':       '#e67eb3',
      \ 'white':         '#f5f1e3',
      \ 'gray':          '#808080',
      \ 'cyan':          '#75bee1',
      \ 'main-color':    '#e67eb3',
      \ 'ui-elements':   '#2a2c2c',
      \ }

function! s:HL(group, fg, bg, attr)
  execute 'hi' a:group
        \ 'guifg=' . (a:fg ==# '' ? 'NONE' : a:fg)
        \ 'guibg=' . (a:bg ==# '' ? 'NONE' : a:bg)
        \ 'gui='   . (a:attr ==# '' ? 'NONE' : a:attr)
        \ 'cterm=' . (a:attr ==# '' ? 'NONE' : a:attr)
endfunction

call s:HL('StatusLine',      s:palette['white'],       s:palette['ui-elements'], '')
call s:HL('StatusLineNC',    s:palette['gray'],        s:palette['ui-elements'], '')
call s:HL('TabLine',         s:palette['white'],       s:palette['ui-elements'], '')
call s:HL('TabLineSel',      s:palette['ui-elements'], s:palette['white'],       '')
call s:HL('TabLineFill',     '',                       s:palette['ui-elements'], '')
call s:HL('Cursor',          '',                       s:palette['yellow'],      '')
call s:HL('CursorColumn',    '',                       s:palette['gray'],        '')
call s:HL('CursorLine',      '',                       s:palette['gray'],        '')
call s:HL('CursorLineNr',    s:palette['main-color'],  s:palette['black'],       'bold')
call s:HL('FoldColumn',      '',                       '',                       '')
call s:HL('Folded',          s:palette['gray'],        '',                       '')
call s:HL('LineNr',          s:palette['gray'],        '',                       '')
call s:HL('MatchParen',      s:palette['black'],       s:palette['yellow'],      'bold')
call s:HL('NonText',         s:palette['gray'],        '',                       '')
call s:HL('SpecialKey',      s:palette['gray'],        '',                       '')
call s:HL('Normal',          s:palette['white'],       s:palette['black'],       '')
call s:HL('Pmenu',           s:palette['white'],       s:palette['ui-elements'], '')
call s:HL('PmenuSel',        s:palette['black'],       s:palette['main-color'],  '')
call s:HL('PmenuThumb',      s:palette['black'],       s:palette['main-color'],  '')
call s:HL('QuickFixLine',    s:palette['ui-elements'], s:palette['main-color'],  '')
call s:HL('SignColumn',      s:palette['black'],       '',                       '')
call s:HL('Title',           s:palette['white'],       '',                       'bold')
call s:HL('VertSplit',       s:palette['ui-elements'], s:palette['ui-elements'], '')
call s:HL('Visual',          s:palette['yellow'],      s:palette['ui-elements'], '')
call s:HL('qfFileName',      s:palette['green'],       '',                       '')
call s:HL('qfLineNr',        s:palette['gray'],        '',                       '')
call s:HL('ErrorMsg',        s:palette['red'],         '',                       '')
call s:HL('SpellBad',        s:palette['red'],         '',                       '')
call s:HL('SpellCap',        s:palette['blue'],        '',                       '')
call s:HL('SpellLocal',      s:palette['main-color'],  '',                       '')
call s:HL('SpellRare',       s:palette['main-color'],  '',                       '')
call s:HL('DiffAdd',         s:palette['green'],       '',                       '')
call s:HL('DiffChange',      s:palette['yellow'],      '',                       '')
call s:HL('DiffDelete',      s:palette['red'],         '',                       '')
call s:HL('diffAdded',       s:palette['green'],       '',                       '')
call s:HL('diffRemoved',     s:palette['red'],         '',                       '')
call s:HL('DiffChange',      s:palette['yellow'],      '',                       '')
call s:HL('DiffDelete',      s:palette['red'],         '',                       '')
call s:HL('DiffText',        '',                       s:palette['ui-elements'], '')
call s:HL('Boolean',         s:palette['main-color'],  '',                       '')
call s:HL('Comment',         s:palette['gray'],        '',                       'italic')
call s:HL('Constant',        s:palette['main-color'],  '',                       '')
call s:HL('Function',        s:palette['white'],       '',                       '')
call s:HL('Identifier',      s:palette['main-color'],  '',                       '')
call s:HL('Keyword',         s:palette['main-color'],  '',                       '')
call s:HL('Number',          s:palette['green'],       '',                       '')
call s:HL('PreProc',         s:palette['yellow'],      '',                       '')
call s:HL('Special',         s:palette['main-color'],  '',                       '')
call s:HL('Statement',       s:palette['main-color'],  '',                       '')
call s:HL('String',          s:palette['green'],       '',                       '')
call s:HL('Todo',            s:palette['main-color'],  '',                       '')
call s:HL('Type',            s:palette['main-color'],  '',                       '')
call s:HL('FzfBorder',       s:palette['green'],       '',                       '')
call s:HL('DirvishPathTail', s:palette['blue'],        '',                       'bold')
call s:HL('PicolineNRM',     s:palette['white'],       s:palette['ui-elements'], '')
call s:HL('PicolineINS',     s:palette['main-color'],  s:palette['ui-elements'], '')
call s:HL('PicolineVIS',     s:palette['main-color'],  s:palette['ui-elements'], '')
call s:HL('PicolineCMD',     s:palette['main-color'],  s:palette['ui-elements'], '')
call s:HL('PicolineRPL',     s:palette['main-color'],  s:palette['ui-elements'], '')
call s:HL('PicolineSEL',     s:palette['main-color'],  s:palette['ui-elements'], '')
call s:HL('PicolineTRM',     s:palette['main-color'],  s:palette['ui-elements'], '')
call s:HL('PicolinePRO',     s:palette['main-color'],  s:palette['ui-elements'], '')
call s:HL('PicolineEXT',     s:palette['main-color'],  s:palette['ui-elements'], '')
call s:HL('PicolinePEN',     s:palette['main-color'],  s:palette['ui-elements'], '')
call s:HL('PicolineSCH',     s:palette['main-color'],  s:palette['ui-elements'], '')

" Set ANSI colors 0-15 for vim's :terminal
let g:terminal_ansi_colors = [
      \ s:palette['black'],
      \ s:palette['red'],
      \ s:palette['green'],
      \ s:palette['yellow'],
      \ s:palette['blue'],
      \ s:palette['magenta'],
      \ s:palette['cyan'],
      \ s:palette['white'],
      \ s:palette['black'],
      \ s:palette['red'],
      \ s:palette['green'],
      \ s:palette['yellow'],
      \ s:palette['blue'],
      \ s:palette['magenta'],
      \ s:palette['cyan'],
      \ s:palette['white'],
      \ ]

" JSX/TSX tags
hi def link htmlTag     Comment
hi def link htmlTagName Identifier

" vim: sw=2 ts=2 et
