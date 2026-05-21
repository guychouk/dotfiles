" Vim compiler file
" Compiler:    nob for C

if exists("current_compiler") | finish | endif
let current_compiler = "nob"

CompilerSet makeprg=./nob
CompilerSet errorformat=
      \%f:%l:%c:\ %t%*[^:]:\ %m
