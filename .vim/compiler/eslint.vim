" Vim compiler file
" Compiler:    ESLint for JavaScript

if exists("current_compiler")
  finish
endif
let current_compiler = "eslint"

if exists(":CompilerSet") != 2
  command -nargs=* CompilerSet setlocal <args>
endif

CompilerSet makeprg=eslint\ --format\ unix\ .
CompilerSet errorformat=
      \%f:%l:%c:\ %m,
      \%-G\\s%#,
      \%-G%*\\d\ problem%.%#
