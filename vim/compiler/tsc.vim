" Vim compiler file
" Compiler:    TypeScript

if exists("current_compiler") | finish | endif
let current_compiler = "tsc"

CompilerSet makeprg=./node_modules/.bin/tsc\ --noEmit\ --pretty\ false
CompilerSet errorformat=
      \%f(%l%*[^0-9]%c):\ %m,
      \%-G%.%#
