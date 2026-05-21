" Vim compiler file
" Compiler:    bun

if exists("current_compiler") | finish | endif
let current_compiler = "bun"

CompilerSet makeprg=bun\ %
CompilerSet errorformat=
      \%f:%l:%c:\ %m,
      \%f:%l,
      \%*\\s%*[^(](%f:%l:%c),
      \%-G%.%#
