" Vim compiler file
" Compiler:    nob

if exists("current_compiler") | finish | endif
let current_compiler = "nob"

CompilerSet makeprg=./nob
CompilerSet errorformat=
      \%f:%l:%c:\ %t%*[^:]:\ %m,
      \%f:%l:%c:\ %m,
      \%f:%l:%c,
      \%f:%l:\ %m,
      \%f:%l
