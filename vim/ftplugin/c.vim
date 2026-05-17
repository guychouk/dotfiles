setlocal foldtext=s:FoldText()

function! s:FoldText() abort
  let l:line = getline(v:foldstart)
  if l:line =~# 'typedef\s\+struct'
    let l:last = getline(v:foldend)
    let l:match = matchstr(l:last, '\k\+\s*;')
    if !empty(l:match)
      let l:name = matchstr(l:match, '^\k\+')
      return l:line . ' ... } ' . l:name . ';'
    endif
  endif
  return foldtext()
endfunction
