" Vim compiler file
" Compiler:    eslint

if exists("current_compiler") | finish | endif
let current_compiler = "eslint"

CompilerSet makeprg=eslint\ --format\ unix\ .
CompilerSet errorformat=
      \%f:%l:%c:\ %m,
      \%-G\\s%#,
      \%-G%*\\d\ problem%.%#
