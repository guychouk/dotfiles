" mdlink.vim - Follow markdown links
" Opens HTTP/HTTPS URLs with macOS `open`, local paths in Vim.

function! s:FollowMdLink() abort
  let line = getline('.')
  let col = col('.')

  " Find all [text](target) on the current line, pick the one under cursor
  let target = ''
  let start = 0
  while 1
    let mstart = match(line, '\[.\{-}\](.\{-})', start)
    if mstart == -1
      break
    endif
    let mtext = matchstr(line, '\[.\{-}\](.\{-})', start)
    let mend = mstart + len(mtext)
    if col >= mstart + 1 && col <= mend
      let target = matchstr(mtext, '(\zs.\{-}\ze)$')
      break
    endif
    let start = mend
  endwhile

  if empty(target)
    echohl WarningMsg | echo 'No markdown link under cursor' | echohl None
    return
  endif

  if target =~# '^https\?://'
    call system('open ' . shellescape(target))
  else
    " Resolve relative to current file's directory
    let dir = expand('%:p:h')
    let path = simplify(dir . '/' . target)
    execute 'edit ' . fnameescape(path)
  endif
endfunction

nnoremap <silent> <Plug>(MdLinkFollow) :<C-u>call <sid>FollowMdLink()<CR>

augroup mdlink
  autocmd!
  autocmd FileType markdown nmap <buffer> gf <Plug>(MdLinkFollow)
augroup END
