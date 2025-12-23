if exists("b:current_syntax")
  finish
endif

" Basic syntax highlighting for palette files
" Treat lines as color definitions
syntax match paletteColor /#\x\{6}\>/
syntax match paletteColor /#\x\{3}\>/
syntax match paletteColor /rgb(\d\+,\s*\d\+,\s*\d\+)/
syntax match paletteColor /rgba(\d\+,\s*\d\+,\s*\d\+,\s*\d\+\(\.\d\+\)\?)/

" Comments
syntax match paletteComment /^#.*/ contains=paletteColor
syntax match paletteComment /\s#.*/ contains=paletteColor

highlight link paletteComment Comment

let b:current_syntax = "palette"
