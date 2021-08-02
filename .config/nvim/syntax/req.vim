hi ReqDescription ctermfg=gray guifg=gray
hi HttpCmd ctermfg=darkgreen guifg=darkgreen
hi Host ctermfg=darkcyan guifg=darkcyan

syn keyword HttpCmd http nextgroup=Host skipwhite
syn match Host '.*:\+\d*'
syn match ReqDescription '#.*'
