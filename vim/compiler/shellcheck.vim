" Vim compiler file
" Compiler:    Shellcheck

if exists("current_compiler") | finish | endif
let current_compiler = "shellcheck"

CompilerSet makeprg=shellcheck\ --format\ gcc\ %
CompilerSet errorformat=
      \%f:%l:%c:\ %tarning:\ %m,
      \%f:%l:%c:\ %trror:\ %m,
      \%f:%l:%c:\ %tote:\ %m,
      \%f:%l:%c:\ %m,
      \%-G\\s%#,
      \%-G%*\\d\ problem%.%#
