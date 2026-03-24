" Vim compiler file
" Compiler: golangci-lint

CompilerSet makeprg=golangci-lint\ run\ ./...
CompilerSet errorformat=
      \%f:%l:%c:\ %m,
      \%-G%.%#
