vim9script

var statusline_mode_map = {
  'i': 'INSERT',
  '!': 'EXTERNAL',
  't': 'TERMINAL',
  'n': 'NORMAL',  'no': 'NORMAL',
  'R': 'REPLACE', 'Rv': 'REPLACE',
  'c': 'COMMAND', 'cv': 'COMMAND', 'ce': 'COMMAND',
  'r': 'PROMPT',  'rm': 'PROMPT',  'r?': 'PROMPT',
  'v': 'VISUAL',  'V':  'VISUAL',  "\<C-V>": 'VISUAL',
  's': 'SELECT',  'S':  'SELECT',  "<\C-S>": 'SELECT',
}

export def Build(active: bool): string
  var separator = "┊"
  var icon = active ? '(•◡•)' : '(ᴗ˳ᴗ)'

  var fugitive = active && exists('*g:FugitiveHead') == 1
    ? separator .. ' ' .. '%{FugitiveHead()}' 
    : ''
  var gutentags = active && exists('*gutentags#statusline') == 1
    ? separator .. ' ' .. '%{gutentags#statusline()}'
    : ''

  var statusline_hlgroup = active ? '%#StatusLine#' : '%#StatusLineNC#'
  var current_ft = empty(&filetype) ? '?' : '%{&filetype}'

  var statusline_segments = [
    statusline_hlgroup,
    active ? statusline_mode_map[mode()] : ' Zzz ',
    separator,
    fnamemodify(getcwd(), ':t'),
    separator,
    '%=',
    '%{expand("%")} %m %r %h',
    '%=',
    gutentags,
    fugitive,
    separator,
    current_ft,
    separator,
    icon,
  ]
  return join(statusline_segments)
enddef

export def Toggle(): void
  if &laststatus == 2
    set laststatus=0
  else
    set laststatus=2
  endif
enddef
