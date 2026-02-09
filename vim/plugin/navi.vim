function! s:WinMoveOrSplit(key) abort
  let t:curwin = winnr()
  exec "wincmd ".a:key
  if (t:curwin == winnr())
    if (match(a:key,'[jk]'))
      wincmd v
    else
      wincmd s
    endif
    exec "wincmd ".a:key
  endif
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

" http://ddrscott.github.io/blog/2016/vim-toggle-movement/
function! s:ToggleMovement(firstOp, thenOp) abort
  let pos = getpos('.')
  execute "normal! " . a:firstOp
  if pos == getpos('.')
    execute "normal! " . a:thenOp
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
command! -bar -nargs=1 TabMoveOrCreate call <sid>TabMoveOrCreate(<q-args>)
command! -bar -nargs=+ ToggleMovement  call <sid>ToggleMovement(<f-args>)
