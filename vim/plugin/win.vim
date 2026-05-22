function! s:WinMoveOrSplit(key) abort
  let t:curwin = winnr()
  exec "wincmd ".a:key
  if (t:curwin == winnr())
    if a:key =~# '[jk]'
      wincmd s
    else
      wincmd v
    endif
    exec "wincmd ".a:key
  endif
endfunction

function! s:WinZoom() abort
  if exists('t:zoomed') && t:zoomed
    execute t:zoom_winrestcmd
    let t:zoomed = 0
  else
    let t:zoom_winrestcmd = winrestcmd()
    resize
    vertical resize
    let t:zoomed = 1
  endif
endfunction

command! -bar -nargs=0 WinZoom         call <sid>WinZoom()
command! -bar -nargs=1 WinMoveOrSplit  call <sid>WinMoveOrSplit(<q-args>)
