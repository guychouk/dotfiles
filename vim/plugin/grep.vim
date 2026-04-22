" A sane grep plugin taken from romainl's grep.md:
" https://gist.github.com/romainl/56f0c28ef953ffc157f36cc495947ab3
" Uses rg as a grep replacement for .gitignore awareness and speed.

if !executable('rg')
  finish
endif

set grepprg=rg\ --vimgrep\ --smart-case
set grepformat=%f:%l:%c:%m

function! s:Grep(...)
  return system(join([&grepprg] + [expandcmd(join(a:000, ' '))], ' '))
endfunction

command! -nargs=+ -complete=file_in_path -bar Grep  cgetexpr s:Grep(<f-args>) | copen
command! -nargs=+ -complete=file_in_path -bar LGrep lgetexpr s:Grep(<f-args>) | lopen

cnoreabbrev <expr> grep (getcmdtype() ==# ':' && getcmdline() ==# 'grep') ? 'Grep' : 'grep'
