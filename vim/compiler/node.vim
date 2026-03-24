" Vim compiler file
" Compiler:    Node.js

CompilerSet makeprg=node\ %
CompilerSet errorformat=
      \%f:%l:%c:\ %m,
      \%f:%l,
      \%*\\s%*[^(](%f:%l:%c),
      \%-G%.%#
