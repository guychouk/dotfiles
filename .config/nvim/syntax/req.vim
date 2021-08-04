hi ReqDescription ctermfg=gray guifg=gray
hi HttpCmd ctermfg=darkgreen guifg=darkgreen
hi Host ctermfg=darkcyan guifg=darkcyan
hi Header ctermfg=darkyellow guifg=darkyellow

syn keyword HttpCmd http nextgroup=Host skipwhite
syn match Host '[a-zA-Z]*:\d*'
syn match ReqDescription '#.*'
syn region Header start='"' end='"' 

