" Vim compiler file
" Compiler:    ruff

CompilerSet makeprg=ruff\ check\ .
CompilerSet errorformat=
      \%f:%l:%c:\ %m,
      \%-G%.%#
