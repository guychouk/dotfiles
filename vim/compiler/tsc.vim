" Vim compiler file
" Compiler:    TypeScript compiler

CompilerSet makeprg=tsc\ --noEmit
CompilerSet errorformat=
      \%f(%l\,%c):\ %m,
      \%-G%.%#
