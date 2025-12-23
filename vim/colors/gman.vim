set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif
let colors_name = "gman"

" ===== Palette Map =====
let s:palette = {
      \ 'bg0':      '#141414',
      \ 'bg1':      '#242424',
      \ 'bg2':      '#313B40',
      \ 'border':   '#242424',
      \ 'slncbg':   '#242424',
      \ 'slncfg':   '#666666',
      \ 'fg0':      '#DDDECF',
      \ 'fg1':      '#94AAA0',
      \ 'muted':    '#738A8B',
      \ 'disabled': '#424447',
      \ 'red':      '#E67E80',
      \ 'orange':   '#E69875',
      \ 'yellow':   '#DBBC7F',
      \ 'green':    '#B2C98F',
      \ 'cyan':     '#93C9A1',
      \ 'blue':     '#9BB5CF',
      \ 'magenta':  '#D6A0D1',
      \ }

" ===== Helper =====
function! s:HL(group, fg, bg, attr)

  execute 'hi' a:group
        \ 'guifg=' . (a:fg ==# '' ? 'NONE' : s:palette[a:fg])
        \ 'guibg=' . (a:bg ==# '' ? 'NONE' : s:palette[a:bg])
        \ 'gui='   . (a:attr ==# '' ? 'NONE' : a:attr)
        \ 'cterm=' . (a:attr ==# '' ? 'NONE' : a:attr)
endfunction

" ===== General UI Elements =====
call s:HL('StatusLine',   'fg0',      'bg1',     '')
call s:HL('StatusLineNC', 'slncfg',   'slncbg',  '')
call s:HL('TabLine',      'fg0',      'bg1',     '')
call s:HL('TabLineSel',   'border',   'fg0',     '')
call s:HL('TabLineFill',  '',         'border',  '')
call s:HL('Cursor',       '',         'muted',   '')
call s:HL('CursorColumn', '',         'bg2',     '')
call s:HL('CursorLine',   '',         'bg2',     '')
call s:HL('CursorLineNr', 'red',      'bg0',     'bold')
call s:HL('FoldColumn',   '',         '',        '')
call s:HL('Folded',       'red',      '',        '')
call s:HL('LineNr',       'disabled', '',        '')
call s:HL('MatchParen',   'green',    'bg2',     'bold')
call s:HL('NonText',      'muted',    '',        '')
call s:HL('SpecialKey',   'muted',    '',        '')
call s:HL('Normal',       'fg0',      '',        '')
call s:HL('Pmenu',        'fg0',      'bg1',     '')
call s:HL('PmenuSel',     'bg0',      'magenta', '')
call s:HL('QuickFixLine', 'bg1',      'orange',  'bold')
call s:HL('SignColumn',   'bg0',      '',        '')
call s:HL('Title',        'fg0',      '',        'bold')
call s:HL('VertSplit',    'border',   'border',  '')
call s:HL('Visual',       'yellow',   'border',  '')
call s:HL('qfFileName',   'green',    '',        '')
call s:HL('qfLineNr',     'fg1',      '',        '')
call s:HL('ErrorMsg',     'red',      '',        '')

" ===== Diff Colors =====
call s:HL('DiffAdd',    'green',  '',       '')
call s:HL('DiffChange', 'yellow', '',       '')
call s:HL('DiffDelete', 'red',    '',       '')
call s:HL('DiffText',   '',       'border', '')

" ===== Syntax Highlighting =====
call s:HL('Boolean',    'red',     '', '')
call s:HL('Comment',    'muted',   '', 'italic')
call s:HL('Constant',   'red',     '', '')
call s:HL('Function',   'fg0',     '', '')
call s:HL('Identifier', 'orange',  '', '')
call s:HL('Keyword',    'red',     '', '')
call s:HL('Number',     'red',     '', '')
call s:HL('PreProc',    'yellow',  '', '')
call s:HL('Special',    'red',     '', '')
call s:HL('Statement',  'magenta', '', '')
call s:HL('String',     'green',   '', '')
call s:HL('Todo',       'red',     '', '')
call s:HL('Type',       'magenta', '', '')

" ===== Statusline Mode Highlight Groups =====
call s:HL('PicolineNRM', 'fg0',     'bg1', '')
call s:HL('PicolineINS', 'red',     'bg1', '')
call s:HL('PicolineVIS', 'yellow',  'bg1', '')
call s:HL('PicolineCMD', 'orange',  'bg1', '')
call s:HL('PicolineRPL', 'magenta', 'bg1', '')
call s:HL('PicolineSEL', 'blue',    'bg1', '')
call s:HL('PicolineTRM', 'cyan',    'bg1', '')
call s:HL('PicolinePRO', 'green',   'bg1', '')
call s:HL('PicolineEXT', 'border',  'bg1', '')

" ===== JSX/TSX tags =====
hi def link htmlTag     Comment
hi def link htmlTagName Identifier

" ===== Plugins =====
call s:HL('DirvishPathTail', 'blue', '', 'bold')
call s:HL('FzfBorder',       'green','', '')

" ===== Terminal Colors =====
" Set ANSI colors for vim's :terminal to match the colorscheme
let g:terminal_ansi_colors = [
      \ s:palette.bg0,
      \ s:palette.red,
      \ s:palette.green,
      \ s:palette.yellow,
      \ s:palette.blue,
      \ s:palette.magenta,
      \ s:palette.cyan,
      \ s:palette.fg0,
      \ s:palette.muted,
      \ s:palette.red,
      \ s:palette.green,
      \ s:palette.orange,
      \ s:palette.blue,
      \ s:palette.magenta,
      \ s:palette.cyan,
      \ s:palette.fg0
      \ ]

" vim: sw=2 ts=2 et
