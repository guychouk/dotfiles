if exists("current_compiler")
	finish
endif

let current_compiler = "jest"

if exists(":CompilerSet") != 2
	command! -nargs=* CompilerSet setlocal <args>
endif

CompilerSet makeprg=jest\ %
CompilerSet errorformat=%f:\ line\ %l\\,\ col\ %c\\,\ %m
