" Vim compiler file
" Compiler:    Node.js

if exists("current_compiler") | finish | endif
let current_compiler = "node"

CompilerSet makeprg=node\ %
CompilerSet errorformat=
      \%f:%l:%c:\ %m,
      \%f:%l,
      \%*\\s%*[^(](%f:%l:%c),
      \%-G%.%#
