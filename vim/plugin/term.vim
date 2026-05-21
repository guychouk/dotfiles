let s:last = ''

function! s:Term(bang, args) abort
  if a:bang
    if empty(s:last)
      echohl WarningMsg | echo 'nothing to rerun' | echohl None
      return
    endif
    execute 'terminal ++rows=10' s:last
  else
    let s:last = a:args
    execute 'terminal ++rows=10' a:args
  endif
endfunction

command! -nargs=* -bang -complete=shellcmd Term call <sid>Term(<bang>0, <q-args>)
