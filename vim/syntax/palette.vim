" Vim syntax file for .palette files (depends on vim-css-color)

if exists("b:current_syntax")
  finish
endif

syntax match paletteColor /#\x\{6}\>/
syntax match paletteColor /#\x\{3}\>/
syntax match paletteColor /rgb(\d\+,\s*\d\+,\s*\d\+)/
syntax match paletteColor /rgba(\d\+,\s*\d\+,\s*\d\+,\s*\d\+\(\.\d\+\)\?)/
syntax match paletteColor /hsl(\d\+,\s*\d\+%\?,\s*\d\+%\?)/
syntax match paletteColor /hsla(\d\+,\s*\d\+%\?,\s*\d\+%\?,\s*\d\+\(\.\d\+\)\?)/

syntax match paletteComment /^#.*/  contains=paletteColor
syntax match paletteComment /\s#.*/ contains=paletteColor

highlight link paletteComment Comment

let b:current_syntax = "palette"
