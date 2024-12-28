" Vim compiler file
" Compiler:    ESLint for JavaScript

if exists("current_compiler")
  finish
endif
let current_compiler = "eslint"

let s:cpo_save = &cpo
set cpo-=C

CompilerSet makeprg=eslint\ --format\ unix\ .
CompilerSet errorformat=
      \%f:%l:%c:\ %m,
      \%-G\\s%#,
      \%-G%*\\d\ problem%.%#

let &cpo = s:cpo_save
unlet s:cpo_save
