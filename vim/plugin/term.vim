function! s:FishEscape(path) abort
  return '"' . escape(a:path, '"$\') . '"'
endfunction

function! s:ReuseOrCreateTerminal(args) abort
  let term_opts_str = matchstr(a:args, '^\%(++\S\+\s*\)*')
  let cmd_args      = strpart(a:args, len(term_opts_str))
  let expanded      = substitute(cmd_args, '%\%(:[phtre~.]\)*', '\=s:FishEscape(expand(submatch(0)))', 'g')

  let rows = matchstr(term_opts_str, '++rows=\zs\d\+')
  let opts = {}

  let term_winnr = -1
  for winnr in range(1, winnr('$'))
    if getwinvar(winnr, '&buftype') ==# 'terminal'
      let term_winnr = winnr
      break
    endif
  endfor

  if term_winnr != -1
    execute term_winnr . 'wincmd w'
    let opts.curwin = 1
  else
    let opts.term_rows = empty(rows) ? 10 : str2nr(rows)
  endif

  call term_start(empty(expanded) ? 'fish' : ['fish', '-c', expanded], opts)
endfunction

command! -nargs=* -complete=shellcmd Term call <sid>ReuseOrCreateTerminal(<q-args>)
