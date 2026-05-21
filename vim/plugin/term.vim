let s:term_args = ''

function! s:ShellEscape(path) abort
  return '"' . escape(a:path, '"$\`') . '"'
endfunction

function! s:Term(bang, args) abort
  if a:bang
    if empty(s:term_args)
      echohl WarningMsg | echo 'nothing to rerun' | echohl None
      return
    endif
    let l:args = s:term_args
  else
    let l:args = a:args
    let s:term_args = a:args
  endif

  let l:opts_str = matchstr(l:args, '^\%(++\S\+\s*\)*')
  let l:cmd_args = strpart(l:args, len(l:opts_str))
  let l:expanded = substitute(l:cmd_args, '%\%(:[phtre~.]\)*', '\=s:ShellEscape(expand(submatch(0)))', 'g')

  let l:rows = matchstr(l:opts_str, '++rows=\zs\d\+')
  let l:opts = {'term_rows': empty(l:rows) ? 10 : str2nr(l:rows)}

  let l:shell = empty($SHELL) ? '/bin/sh' : $SHELL
  call term_start(empty(l:expanded) ? l:shell : [l:shell, '-c', l:expanded], l:opts)
endfunction

command! -nargs=* -bang -complete=shellcmd Term call <sid>Term(<bang>0, <q-args>)
