" Vim compiler file
" Compiler:    grep

CompilerSet makeprg=grep\ -rn\ $*\ .
CompilerSet errorformat=%f:%l:%m
