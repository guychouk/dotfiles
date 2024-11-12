if exists("shellcheck")
  finish
endif

let current_compiler = "shellcheck"

let s:cpo_save = &cpo
set cpo-=C

CompilerSet makeprg=shellcheck\ -f\ gcc\ -s\ bash\ %
CompilerSet errorformat=%f:%l:%c:\ %t%*[^:]:\ %m

let &cpo = s:cpo_save
unlet s:cpo_save
