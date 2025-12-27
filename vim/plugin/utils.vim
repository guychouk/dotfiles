function! s:CleanUndoFiles()
  !find ~/.vim/tmp/undo -type f -mtime +100d \! -name '.gitignore' -delete
endfunction

function! s:AddOpenBuffersToList()
  let loc_list = map(filter(range(1, bufnr('$')), 'buflisted(v:val)'), '{"bufnr": v:val}')
  call setloclist(0, loc_list)
  lopen
endfunction

function! s:RemoveQfItem()
  let curqfidx = line('.') - 1
  let qfall = getqflist()
  call remove(qfall, curqfidx)
  call setqflist(qfall, 'r')
  execute curqfidx + 1 . "cfirst"
  :copen
endfunction

function! s:WinMoveOrSplit(key)
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

function! s:ZoomWin()
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

function! s:SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunction

function! s:InsertRelativeFilePath()
  let l:cwd = getcwd()
  let l:file = expand('%:p')
  if l:file == ''
    echo "No file associated with this buffer"
    return
  endif
  let l:relative_path = substitute(l:file, l:cwd . '/', '', '')
  execute "normal! i" . "[" . l:relative_path . "] \<ESC>"
endfunction

function! s:CopyRelativeFilePath()
  let l:root = getcwd()
  let l:file = expand('%:p')
  if l:file == ''
    echo "No file associated with this buffer"
    return
  endif
  let l:relative_path = substitute(l:file, l:root . '/', '', '')
  let @+ = l:relative_path
  echo "Copied: " . l:relative_path
endfunction

function! s:CleanupHelpBuffers()
  for buf in getbufinfo({'buflisted': 1})
    if getbufvar(buf.bufnr, '&filetype') ==# 'help'
      execute 'bwipeout' buf.bufnr
    endif
  endfor
  echomsg "Help buffers cleaned up"
endfunction

function! s:ReuseOrCreateTerminal(args) abort
  let expanded_args = substitute(a:args, '%', expand('%'), 'g')
  " Find first visible terminal window
  let term_winnr = -1
  for winnr in range(1, winnr('$'))
    if getwinvar(winnr, '&buftype') ==# 'terminal'
      let term_winnr = winnr
      break
    endif
  endfor
  if term_winnr != -1
    " Terminal exists - reuse it
    execute term_winnr . 'wincmd w'
    execute 'term ++curwin ' . expanded_args
  else
    " No terminal - create new
    execute 'term ' . expanded_args
  endif
endfunction

" http://ddrscott.github.io/blog/2016/vim-toggle-movement/
function! s:ToggleMovement(firstOp, thenOp)
  let pos = getpos('.')
  execute "normal! " . a:firstOp
  if pos == getpos('.')
    execute "normal! " . a:thenOp
  endif
endfunction

command! -nargs=* -complete=shellcmd Term                   call <sid>ReuseOrCreateTerminal(<q-args>)
command! -bar     -nargs=+           ToggleMovement         call <sid>ToggleMovement(<f-args>)
command! -bar     -nargs=1           WinMoveOrSplit         call <sid>WinMoveOrSplit(<q-args>)
command! -bar     -nargs=0           ZoomWin                call <sid>ZoomWin()
command! -bar     -nargs=0           SynStack               call <sid>SynStack()
command! -bar     -nargs=0           RemoveQfItem           call <sid>RemoveQfItem()
command! -bar     -nargs=0           CleanUndoFiles         call <sid>CleanUndoFiles()
command! -bar     -nargs=0           AddOpenBuffersToList   call <sid>AddOpenBuffersToList()
command! -bar     -nargs=0           CopyRelativeFilePath   call <sid>CopyRelativeFilePath()
command! -bar     -nargs=0           InsertRelativeFilePath call <sid>InsertRelativeFilePath()
command! -bar     -nargs=0           CleanupHelp            call <sid>CleanupHelpBuffers()
