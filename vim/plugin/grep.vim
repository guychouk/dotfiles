" A sane grep plugin taken from romainl's grep.md:
" https://gist.github.com/romainl/56f0c28ef953ffc157f36cc495947ab3
" Uses rg as a grep replacement for .gitignore awareness and speed.

if !executable('rg')
  finish
endif

set grepprg=rg\ --vimgrep\ --smart-case
set grepformat=%f:%l:%c:%m

function! s:grep(...)
  let lines = systemlist(&grepprg . ' ' . join(map(copy(a:000), {_, v -> shellescape(v)})))
  return type(lines) == v:t_list ? lines : []
endfunction

command! -nargs=+ -complete=file_in_path -bar Grep  cgetexpr s:grep(<f-args>)
command! -nargs=+ -complete=file_in_path -bar LGrep lgetexpr s:grep(<f-args>)

cnoreabbrev <expr> grep (getcmdtype() ==# ':' && getcmdline() ==# 'grep') ? 'Grep' : 'grep'
