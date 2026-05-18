if !exists('g:tabline_custom_names')
  let g:tabline_custom_names = {}
endif

function! Picotab() abort
  let s = ''
  for i in range(tabpagenr('$'))
    if i + 1 == tabpagenr()
      let s .= '%#TabLineSel#'
    else
      let s .= '%#TabLine#'
    endif
    let s .= '%' . (i + 1) . 'T'
    let buflist = tabpagebuflist(i + 1)
    let winnr = tabpagewinnr(i + 1)
    if has_key(g:tabline_custom_names, i + 1)
      let name = g:tabline_custom_names[i + 1]
    else
      let name = fnamemodify(bufname(buflist[winnr - 1]), ':t')
      if empty(name)
        let name = ' ? '
      endif
    endif
    let s .= ' ' . name . ' '
  endfor
  let s .= '%#TabLineFill#%T'
  return s
endfunction

function! s:RenameCurrentTab(new_name)
  let g:tabline_custom_names[tabpagenr()] = a:new_name
endfunction

function! s:TabMoveOrCreate(key) abort
  if a:key ==# 'h'
    if tabpagenr() == 1
      0tabnew
    else
      normal! gT
    endif
  else
    if tabpagenr() == tabpagenr('$')
      tabnew
    else
      normal! gt
    endif
  endif
endfunction

command! -bar -nargs=1 RenameTab       call <sid>RenameCurrentTab(<q-args>)
command! -bar -nargs=1 TabMoveOrCreate call <sid>TabMoveOrCreate(<q-args>)

set tabline=%!Picotab()
