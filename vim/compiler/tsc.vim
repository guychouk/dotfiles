" Vim compiler file
" Compiler:    TypeScript compiler

if exists("current_compiler") | finish | endif
let current_compiler = "tsc"

CompilerSet makeprg=tsc\ --noEmit
CompilerSet errorformat=
      \%f(%l\,%c):\ %m,
      \%-G%.%#
