" Vim compiler file
" Compiler:    Python 3

CompilerSet makeprg=python3\ %
CompilerSet errorformat=
      \%*\\sFile\ "%f"\,\ line\ %l\,\ in\ %m,
      \%-G%.%#
