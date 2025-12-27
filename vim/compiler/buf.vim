" Vim compiler file
" Compiler:    buf compiler

CompilerSet makeprg=buf
CompilerSet errorformat=
      \%f:%l:%c:%m,
      \%-G\\s%#,
      \%-G%*\\d\ problem%.%#
