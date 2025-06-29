vim9script

var statusline_mode_map = {
  'i':      'INS',
  '!':      'EXT',
  't':      'TRM',
  'n':      'NRM',
  'no':     'NRM',
  'R':      'RPL',
  'Rv':     'RPL',
  'c':      'CMD',
  'cv':     'CMD',
  'ce':     'CMD',
  'r':      'PRO',
  'rm':     'PRO',
  'r?':     'PRO',
  'v':      'VIS',
  'V':      'VIS',
  "\<C-V>": 'VIS',
  's':      'SEL',
  'S':      'SEL',
  "<\C-S>": 'SEL',
}

export def Build(active: bool): string
  var separator = "┊"
  var icon = active ? '(ᵔ◡ᵔ)' : '(∪｡∪)'

  var fugitive = active && exists('*g:FugitiveHead') == 1
    ? separator .. ' ' .. '%{FugitiveHead()}' 
    : ''
  var gutentags = active && exists('*gutentags#statusline') == 1
    ? separator .. ' ' .. '%{gutentags#statusline()}'
    : ''

  var statusline_hlgroup = active ? '%#StatusLine#' : '%#StatusLineNC#'
  var current_ft = empty(&filetype) ? '?' : '%{&filetype}'

  var statusline_segments = [
    active ? '%#StatusLine' .. statusline_mode_map[mode()] .. '#' : '',
    active ? statusline_mode_map[mode()] : 'ZZZ',
    statusline_hlgroup,
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
