" Vim compiler file
" Compiler:    bun

CompilerSet makeprg=bun\ %
CompilerSet errorformat=
      \%f:%l:%c:\ %m,
      \%f:%l,
      \%*\\s%*[^(](%f:%l:%c),
      \%-G%.%#
