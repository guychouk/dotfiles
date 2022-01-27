runtime syntax/zsh.vim

hi ReqDescription ctermfg=gray       guifg=gray
hi HttpCmd        ctermfg=darkgreen  guifg=darkgreen
hi Host           ctermfg=darkcyan   guifg=darkcyan
hi Header         ctermfg=darkyellow guifg=darkyellow

syn keyword HttpCmd http nextgroup=Host skipwhite

let b:current_syntax = "repl"
