" Vim compiler file
" Compiler:    node

if exists("current_compiler") | finish | endif
let current_compiler = "node"

CompilerSet makeprg=node\ %
CompilerSet errorformat=
      \%f:%l:%c:\ %m,
      \%f:%l,
      \%*\\s%*[^(](%f:%l:%c),
      \%-G%.%#
