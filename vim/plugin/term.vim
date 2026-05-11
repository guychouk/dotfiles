let s:term_cmd = ''

function! s:ShellEscape(path) abort
  return '"' . escape(a:path, '"$\') . '"'
endfunction

function! s:ReuseOrCreateTerminal(args) abort
  let orig_winnr = winnr()
  let term_opts_str = matchstr(a:args, '^\%(++\S\+\s*\)*')
  let cmd_args      = strpart(a:args, len(term_opts_str))
  let expanded      = substitute(cmd_args, '%\%(:[phtre~.]\)*', '\=s:ShellEscape(expand(submatch(0)))', 'g')

  let rows = matchstr(term_opts_str, '++rows=\zs\d\+')
  let opts = {}

  let term_buf = -1
  for buf in term_list()
    if !empty(buf)
      let term_buf = buf
      break
    endif
  endfor

  if !empty(expanded)
    let s:term_cmd = expanded
    if term_buf != -1
      call term_sendkeys(term_buf, expanded . "\n")
      return
    endif
  endif

  if term_buf != -1
    for winnr in range(1, winnr('$'))
      if winbufnr(winnr) == term_buf
        execute winnr . 'wincmd w'
        let opts.curwin = 1
        break
      endif
    endfor
  else
    let opts.term_rows = empty(rows) ? 10 : str2nr(rows)
  endif

  let shell = empty($SHELL) ? '/bin/sh' : $SHELL
  call term_start(empty(expanded) ? shell : [shell, '-c', expanded], opts)

  execute orig_winnr . 'wincmd w'
endfunction

function! s:Term(bang, args) abort
  if a:bang
    if empty(s:term_cmd)
      echohl WarningMsg
      echo 'nothing to rerun'
      echohl None
      return
    endif
    call s:ReuseOrCreateTerminal(s:term_cmd)
  else
    call s:ReuseOrCreateTerminal(a:args)
  endif
endfunction

command! -nargs=* -bang -complete=shellcmd Term call <sid>Term(<bang>0, <q-args>)
