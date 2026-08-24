" Label each tab with its working directory (the dir set via :tcd, else the
" global cwd) rather than vim's default pathshortened buffer name.

function! Tabline() abort
  let s = ''
  for i in range(1, tabpagenr('$'))
    let cwd = getcwd(tabpagewinnr(i), i)
    let name = fnamemodify(cwd, ':t')
    if empty(name)
      let name = cwd
    endif
    let s .= (i == tabpagenr() ? '%#TabLineSel#' : '%#TabLine#')
    let s .= '%' . i . 'T ' . name . ' '
  endfor
  return s . '%#TabLineFill#%T'
endfunction

set tabline=%!Tabline()

augroup tabline_dirchanged
  autocmd!
  autocmd DirChanged * redrawtabline
augroup END
