" Vim compiler plugin
" Compiler:    nob for C

if exists("current_compiler")
  finish
endif
let current_compiler = "nob"

let s:cpo_save = &cpo
set cpo-=C

CompilerSet makeprg=./nob
CompilerSet errorformat=%f:%l:%c:\ %t%*[^:]:\ %m

let &cpo = s:cpo_save
unlet s:cpo_save
