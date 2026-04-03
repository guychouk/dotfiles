" Vim compiler file
" Compiler: gotest

CompilerSet makeprg=go\ test\ ./...
CompilerSet errorformat=
      \%f:%l:\ %m,
      \%-G%.%#
