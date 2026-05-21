" Vim compiler file
" Compiler:    Python 3

if exists("current_compiler") | finish | endif
let current_compiler = "python3"

CompilerSet makeprg=python3\ %
CompilerSet errorformat=
      \%*\\sFile\ "%f"\,\ line\ %l\,\ in\ %m,
      \%-G%.%#
