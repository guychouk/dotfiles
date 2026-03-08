function! s:ReuseOrCreateTerminal(args) abort
  let expanded_args = substitute(a:args, '%', expand('%'), 'g')
  let cmd = empty(expanded_args) ? 'fish' : 'fish -c ' . shellescape(expanded_args)
  let term_winnr = -1
  for winnr in range(1, winnr('$'))
    if getwinvar(winnr, '&buftype') ==# 'terminal'
      let term_winnr = winnr
      break
    endif
  endfor
  if term_winnr != -1
    execute term_winnr . 'wincmd w'
    execute 'term ++curwin ' . cmd
  else
    execute 'term ++rows=10 ' . cmd
  endif
endfunction

command! -nargs=* -complete=shellcmd Term call <sid>ReuseOrCreateTerminal(<q-args>)
