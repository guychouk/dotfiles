function! s:ReuseOrCreateTerminal(args) abort
  let expanded_args = substitute(a:args, '%', expand('%'), 'g')
  let term_winnr = -1
  for winnr in range(1, winnr('$'))
    if getwinvar(winnr, '&buftype') ==# 'terminal'
      let term_winnr = winnr
      break
    endif
  endfor
  if term_winnr != -1
    execute term_winnr . 'wincmd w'
    execute 'term ++curwin ' . expanded_args
  else
    execute 'term ' . expanded_args
  endif
endfunction

function! s:SynStack() abort
  if !exists("*synstack")
    echom "No synstack function available!"
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunction

command! -bar     -nargs=0           SynStack call <sid>SynStack()
command! -nargs=* -complete=shellcmd Term     call <sid>ReuseOrCreateTerminal(<q-args>)
