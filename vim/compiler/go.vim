" Vim compiler file
" Compiler:    go

if exists("current_compiler") | finish | endif
let current_compiler = "go"

CompilerSet makeprg=go\ build\ ./...
CompilerSet errorformat=
      \%f:%l:%c:\ %m,
      \%-G%.%#
