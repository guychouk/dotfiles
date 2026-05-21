" Vim compiler file
" Compiler:    grep

if exists("current_compiler") | finish | endif
let current_compiler = "grep"

CompilerSet makeprg=grep\ -rn\ $*\ .
CompilerSet errorformat=
      \%f:%l:%m
