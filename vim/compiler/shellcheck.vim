" Vim compiler file
" Compiler:    Shellcheck

CompilerSet makeprg=shellcheck\ %
CompilerSet errorformat=
      \%f:%l:%c:\ %m,
      \%-G\\s%#,
      \%-G%*\\d\ problem%.%#
