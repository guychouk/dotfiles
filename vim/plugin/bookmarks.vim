function! s:Bookmark() range abort
  let l:file = expand('%:p')
  if l:file == ''
    echo "No file associated with this buffer"
    return
  endif
  let l:quote = ''
  let l:is_visual = line("'<") == a:firstline && line("'>") == a:lastline
  if l:is_visual && a:firstline == a:lastline
    let l:location = l:file . ':' . a:firstline . ':' . col("'<")
    let l:selected = strpart(getline(a:firstline), col("'<") - 1, col("'>") - col("'<") + 1)
    let l:quote = '"' . trim(l:selected) . '"'
  elseif l:is_visual
    let l:location = l:file . ':' . a:firstline . '-' . a:lastline
    let l:lines = getline(a:firstline, a:lastline)
    let l:quote = '"' . join(map(l:lines, 'trim(v:val)'), ' ') . '"'
  else
    let l:location = l:file . ':' . a:firstline . ':' . col('.')
  endif
  let l:comment = input('💬: ')
  if l:comment == ''
    return
  endif
  let l:entry = l:location
  if l:quote != ''
    let l:entry .= ' - ' . l:quote
  endif
  let l:entry .= ' - ' . l:comment
  call writefile([l:entry], getcwd() . '/BOOKMARKS.md', 'a')
endfunction

function! s:OpenBookmark() abort
  let l:match = matchlist(getline('.'), '^\(.\{-\}\):\(\d\+\):\(\d\+\)')
  if empty(l:match)
    normal! gf
    return
  endif
  execute 'edit ' . fnameescape(l:match[1])
  call cursor(str2nr(l:match[2]), str2nr(l:match[3]))
endfunction

augroup bookmarks
  autocmd!
  autocmd BufRead BOOKMARKS.md nnoremap <buffer> gf :call <sid>OpenBookmark()<CR>
augroup END

command! -range Bookmark <line1>,<line2>call <sid>Bookmark()
