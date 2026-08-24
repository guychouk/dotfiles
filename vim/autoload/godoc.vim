" Render `go doc` into a reusable scratch window instead of the blocking
" pager, bound to K via :GoKeywordPrg. iskeyword is widened so the word
" under the cursor includes the package selector (fmt.Println, not just
" Println); -C anchors go doc in the file's package so imports and
" same-package names resolve.
function! godoc#open() abort
  let l:isk = &l:iskeyword
  setlocal iskeyword+=.
  let l:word = expand('<cword>')
  let &l:iskeyword = l:isk
  let l:out = systemlist(join(map(['go', 'doc', '-C', expand('%:p:h'), l:word], {_, v -> shellescape(v)})))
  if v:shell_error != 0
    echohl WarningMsg | echo 'go doc: ' . join(l:out, ' ') | echohl None
    return
  endif
  for l:w in range(1, winnr('$'))
    if getbufvar(winbufnr(l:w), 'godoc_scratch', 0)
      execute l:w . 'wincmd w' | break
    endif
  endfor
  if !get(b:, 'godoc_scratch', 0)
    botright new
    let b:godoc_scratch = 1
    setlocal buftype=nofile bufhidden=hide noswapfile nobuflisted winfixheight
    setlocal syntax=go
    nnoremap <buffer> <silent> q <C-w>c
  endif
  setlocal modifiable
  silent %delete _
  call setline(1, l:out)
  setlocal nomodifiable
  execute 'resize' min([max([len(l:out), 3]), 20])
  normal! gg
endfunction
