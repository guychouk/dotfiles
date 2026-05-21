" Vim compiler file
" Compiler:    buf compiler

if exists("current_compiler") | finish | endif
let current_compiler = "buf"

CompilerSet makeprg=buf
CompilerSet errorformat=
      \%f:%l:%c:%m,
      \%-G\\s%#,
      \%-G%*\\d\ problem%.%#
