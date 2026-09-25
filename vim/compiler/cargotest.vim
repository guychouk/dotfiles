" Vim compiler file
" Compiler:    cargo test

if exists("current_compiler") | finish | endif
runtime compiler/cargo.vim
unlet current_compiler
let current_compiler = "cargotest"

CompilerSet makeprg=cargo\ test\ --color=never
CompilerSet errorformat^=thread\ '%m'\ %.%#panicked\ at\ %f:%l:%c:
