" Vim compiler file
" Compiler:    ruff

if exists("current_compiler") | finish | endif
let current_compiler = "ruff"

CompilerSet makeprg=ruff\ check\ -q\ --output-format\ concise
CompilerSet errorformat=
      \%f:%l:%c:\ %m,
      \%-G%.%#
