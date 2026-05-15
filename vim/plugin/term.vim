let s:term_cmd = ''

function! s:ShellEscape(path) abort
  return '"' . escape(a:path, '"$\') . '"'
endfunction

function! s:ReuseOrCreateTerminal(args) abort
  let term_opts_str = matchstr(a:args, '^\%(++\S\+\s*\)*')
  let cmd_args      = strpart(a:args, len(term_opts_str))
  let expanded      = substitute(cmd_args, '%\%(:[phtre~.]\)*', '\=s:ShellEscape(expand(submatch(0)))', 'g')

  let rows = matchstr(term_opts_str, '++rows=\zs\d\+')
  let opts = {}

  if !empty(expanded)
    let s:term_cmd = expanded
    let term_bufs = term_list()
    if !empty(term_bufs)
      call term_sendkeys(term_bufs[0], expanded . "\n")
      return
    endif
  endif

  if empty(rows)
    let opts.term_rows = 10
  else
    let opts.term_rows = str2nr(rows)
  endif

  let shell = empty($SHELL) ? '/bin/sh' : $SHELL
  call term_start(empty(expanded) ? shell : [shell, '-c', expanded], opts)
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
