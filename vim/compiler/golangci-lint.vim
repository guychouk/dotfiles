" Vim compiler file
" Compiler: golangci-lint

if exists("current_compiler") | finish | endif
let current_compiler = "golangci-lint"

CompilerSet makeprg=golangci-lint\ run\ ./...
CompilerSet errorformat=
      \%f:%l:%c:\ %m,
      \%-G%.%#
