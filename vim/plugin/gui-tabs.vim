" MacVim's default GUI tab label falls back to the full, untailed buffer
" name whenever fnamemodify(name, ':t') comes back empty -- which it does
" for any trailing-slash bufname (netrw directory buffers, e.g. the tabs
" :SurfDir opens). MacVim then squeezes that whole path to fit the tab
" width instead of truncating, e.g. "/Users/guychouk/src/" -> "/U/g/s/".
" Terminal vim's default 'tabline' tails these correctly; this just makes
" the GUI do the same.

if !has('gui_running')
  finish
endif

function! GuiTabLabel() abort
  let l:buflist = tabpagebuflist(v:lnum)
  let l:winnr = tabpagewinnr(v:lnum)
  let l:bufnr = l:buflist[l:winnr - 1]
  let l:name = substitute(bufname(l:bufnr), '/$', '', '')
  let l:tail = fnamemodify(l:name, ':t')
  let l:label = empty(l:tail) ? '[No Name]' : l:tail

  if len(l:buflist) > 1
    let l:label = len(l:buflist) . ' ' . l:label
  endif
  if getbufvar(l:bufnr, '&modified')
    let l:label .= '+'
  endif
  return l:label
endfunction

set guitablabel=%{GuiTabLabel()}
