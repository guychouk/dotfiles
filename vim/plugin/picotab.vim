" Initialize custom tab names dictionary
if !exists('g:tabline_custom_names')
  let g:tabline_custom_names = {}
endif

" Must be global - called by tabline setting
function! Picotab()
  let s = ''
  for i in range(tabpagenr('$'))
    " Select the highlighting
    if i + 1 == tabpagenr()
      let s .= '%#TabLineSel#'
    else
      let s .= '%#TabLine#'
    endif
    " Set the tab page number (for mouse clicks)
    let s .= '%' . (i + 1) . 'T'
    " Get the buffer list and window number for the tab page
    let buflist = tabpagebuflist(i + 1)
    let winnr = tabpagewinnr(i + 1)
    " Determine the name to use: custom or file name
    if has_key(g:tabline_custom_names, i + 1)
      let name = g:tabline_custom_names[i + 1]
    else
      let name = fnamemodify(bufname(buflist[winnr - 1]), ':t')
      if empty(name)
        let name = ' ? '
      endif
    endif
    " Append the name to the tab line
    let s .= ' ' . name . ' '
  endfor
  " Fill the rest of the tabline
  let s .= '%#TabLineFill#%T'
  return s
endfunction

function! s:RenameCurrentTab(new_name)
  " Store the new name in the dictionary for the current tab
  let g:tabline_custom_names[tabpagenr()] = a:new_name
endfunction

command! -bar -nargs=1 RenameTab call <sid>RenameCurrentTab(<q-args>)
