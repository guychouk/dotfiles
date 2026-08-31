" A simple note taker: one file per project under ~/Documents/notes/

let s:notes_dir = expand('~/Documents/notes')

function! s:notes_file() abort
  let l:dir = expand('%:p:h')
  let l:git = finddir('.git', l:dir . ';')
  if empty(l:git)
    let l:git = findfile('.git', l:dir . ';')
  endif
  if empty(l:git)
    let l:root = l:dir
  else
    let l:git = substitute(fnamemodify(l:git, ':p'), '/$', '', '')
    let l:root = fnamemodify(l:git, ':h')
  endif
  return s:notes_dir . '/' . fnamemodify(l:root, ':t') . '.md'
endfunction

function! s:new_note(first, last, lines) abort
  let l:src = expand('%:p')
  if empty(l:src)
    echohl WarningMsg | echo 'no file' | echohl None
    return
  endif
  let l:target = s:notes_file()
  let l:loc = a:first == a:last
        \ ? l:src . ':' . a:first
        \ : l:src . ':' . a:first . '-' . a:last
  let l:fence = ['```' . &filetype] + a:lines + ['```']

  belowright 10new
  setlocal buftype=acwrite bufhidden=wipe noswapfile filetype=markdown
  execute 'file' fnameescape('note:' . l:loc)
  let b:note_target = l:target
  call setline(1, ['## ' . l:loc, ''] + l:fence + ['', ''])
  call cursor(line('$'), 1)
  augroup notes_buffer
    autocmd! * <buffer>
    autocmd BufWriteCmd <buffer> call s:save_note()
  augroup END
  startinsert
endfunction

function! s:save_note() abort
  let l:lines = getline(1, '$')
  while !empty(l:lines) && empty(l:lines[-1])
    call remove(l:lines, -1)
  endwhile
  if empty(l:lines)
    setlocal nomodified
    bwipeout
    return
  endif
  call mkdir(s:notes_dir, 'p')
  call writefile(l:lines + [''], b:note_target, 'a')
  setlocal nomodified
  bwipeout
endfunction

function! s:new_note_range(first, last) abort
  call s:new_note(a:first, a:last, getline(a:first, a:last))
endfunction

function! s:new_note_visual() abort
  let l:first = line("'<")
  let l:last = line("'>")
  let l:lines = getline(l:first, l:last)
  if visualmode() ==# 'v' && !empty(l:lines)
    let l:endcol = min([col("'>"), strlen(l:lines[-1])])
    let l:lines[-1] = l:lines[-1][: l:endcol - 1]
    let l:lines[0] = l:lines[0][col("'<") - 1:]
  endif
  call s:new_note(l:first, l:last, l:lines)
endfunction

function! s:open_note() abort
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
  autocmd BufRead ~/Documents/notes/*.md nnoremap <buffer> gf :call <sid>open_note()<CR>
augroup END

command! -range NewNote       call <sid>new_note_range(<line1>, <line2>)
command!        NewNoteVisual call <sid>new_note_visual()
command!        Notes         execute 'edit' fnameescape(<sid>notes_file())
