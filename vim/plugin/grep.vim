" A sane grep plugin taken from romainl's grep.md:
" https://gist.github.com/romainl/56f0c28ef953ffc157f36cc495947ab3
" Uses rg as a grep replacement for .gitignore awareness and speed.
"
" The first argument is an rg regex; later arguments are rg flags and paths.
" A space inside a phrase is a regex character, so:
"   :Grep After.settling        one char between words (space, but also AfterXsettling)
"   :Grep After\ settling       exactly one space (backslash-space survives <f-args>)
"   :Grep -U After\s+settling   any whitespace, including across lines

if !executable('rg')
  finish
endif

set grepprg=rg\ --vimgrep\ --smart-case
set grepformat=%f:%l:%c:%m

function! s:grep(...)
  let lines = systemlist(&grepprg . ' ' . join(map(copy(a:000), {_, v -> shellescape(v)})))
  return type(lines) == v:t_list ? lines : []
endfunction

" vim-qf's QuickFixCmdPost autocmd opens and sizes the list window for
" cgetexpr/lgetexpr (see its g:qf_auto_resize and g:qf_max_height), but it
" leaves the cursor in the list window, so all that is left here is landing on
" the first entry. <tag>first moves the cursor into the file window.
function! s:land(tag)
  if !empty(a:tag ==# 'c' ? getqflist() : getloclist(0))
    execute a:tag . 'first'
  endif
endfunction

command! -nargs=+ -complete=file_in_path -bar Grep  cgetexpr s:grep(<f-args>) | call s:land('c')
command! -nargs=+ -complete=file_in_path -bar LGrep lgetexpr s:grep(<f-args>) | call s:land('l')

cnoreabbrev <expr> grep (getcmdtype() ==# ':' && getcmdline() ==# 'grep') ? 'Grep' : 'grep'