" Vim compiler file
" Compiler:    go

CompilerSet makeprg=go\ build\ ./...
CompilerSet errorformat=
      \%f:%l:%c:\ %m,
      \%-G%.%#
