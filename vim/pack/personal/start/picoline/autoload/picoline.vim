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
  var icon = active ? '(•◡•)' : '(ᴗ˳ᴗ)'
  var ft = empty(&filetype) ? '?' : '%{&filetype}'
  var fugitive = active && exists('*g:FugitiveHead') == 1 ? '%{FugitiveHead()}' : ''
  var gutentags = active && exists('*gutentags#statusline') == 1 ? '%{gutentags#statusline()}' : ''
  var current_mode = active ? statusline_mode_map[mode()] : '      '
  var left_separator = active ? '%#PicolineLeftSep#' : ' '
  var right_separator = active ? '%#PicolineRightSep#' : ' '
  var section_hlgroup = active ? '%#PicolineSection#' : '%#StatusLineNC#'
  var statusline_hlgroup = active ? '%#StatusLine#' : '%#StatusLineNC#'
  var statusline_segments = [
    section_hlgroup .. ' ' .. current_mode .. ' ',
    left_separator,
    statusline_hlgroup,
    ' ' .. fnamemodify(getcwd(), ':t') .. '    ' .. '%{expand("%")} %m %r %h',
    '%=',
    gutentags .. ' ',
    fugitive .. ' ' .. right_separator .. section_hlgroup,
    ' ' .. ft .. '  ' .. icon
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
