" Vim compiler file
" Compiler:    ESLint

CompilerSet makeprg=eslint\ --format\ unix\ .
CompilerSet errorformat=
      \%f:%l:%c:\ %m,
      \%-G\\s%#,
      \%-G%*\\d\ problem%.%#
