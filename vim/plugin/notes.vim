function! s:NewNote(first, last) abort
  let l:src = expand('%:p')
  if empty(l:src)
    echohl WarningMsg | echo 'no file' | echohl None
    return
  endif
  let l:target = expand('%:p:h') . '/notes.md'
  let l:loc = a:first == a:last
        \ ? l:src . ':' . a:first
        \ : l:src . ':' . a:first . '-' . a:last
  let l:fence = ['```' . &filetype] + getline(a:first, a:last) + ['```']

  belowright 10new
  setlocal buftype=acwrite bufhidden=wipe noswapfile filetype=markdown
  execute 'file' fnameescape('note:' . l:loc)
  let b:note_target = l:target
  call setline(1, ['## ' . l:loc, ''] + l:fence + ['', ''])
  call cursor(line('$'), 1)
  autocmd BufWriteCmd <buffer> call s:SaveNote()
  startinsert
endfunction

function! s:SaveNote() abort
  let l:lines = getline(1, '$')
  while !empty(l:lines) && empty(l:lines[-1])
    call remove(l:lines, -1)
  endwhile
  if empty(l:lines)
    setlocal nomodified
    bwipeout
    return
  endif
  call writefile(l:lines + [''], b:note_target, 'a')
  setlocal nomodified
  bwipeout
endfunction

function! s:OpenNote() abort
  let l:match = matchlist(getline('.'), '\(\f\+\):\(\d\+\)\%(-\d\+\)\?')
  if empty(l:match)
    normal! gf
    return
  endif
  execute 'edit' fnameescape(l:match[1])
  call cursor(str2nr(l:match[2]), 1)
endfunction

augroup notes
  autocmd!
  autocmd BufRead notes.md nnoremap <buffer> gf :call <sid>OpenNote()<CR>
augroup END

command! -range NewNote call <sid>NewNote(<line1>, <line2>)
command!        Notes execute 'edit' fnameescape(expand('%:p:h') . '/notes.md')
