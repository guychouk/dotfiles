let s:job  = v:null
let s:gen  = 0
let s:cmd  = ''
let s:qfid = 0
let s:efm  = ''

function! s:OnOut(gen, ch, msg) abort
  if a:gen != s:gen | return | endif
  call setqflist([], 'a', {'id': s:qfid, 'lines': [a:msg], 'efm': s:efm})
endfunction

function! s:OnExit(gen, cmd, job, status) abort
  if a:gen != s:gen | return | endif
  call setqflist([], 'r', {'id': s:qfid, 'title': printf('[%d] %s', a:status, a:cmd)})
  cwindow
  echo printf('Compile: %s -> %d', a:cmd, a:status)
endfunction

function! s:Compile(bang, arg) abort
  let l:arg = trim(a:arg)
  if a:bang && empty(l:arg)
    if empty(s:cmd)
      echohl WarningMsg | echo 'nothing to recompile' | echohl None
      return
    endif
    let l:cmd = s:cmd
  else
    let l:cmd = empty(l:arg) ? &makeprg : l:arg
  endif
  let l:cmd = expandcmd(substitute(l:cmd, '\s*\$\*', '', 'g'))
  if empty(l:cmd)
    echohl WarningMsg | echo 'no command' | echohl None
    return
  endif
  if s:job isnot v:null && job_status(s:job) ==# 'run'
    call job_stop(s:job)
  endif
  let s:gen += 1
  let s:cmd  = l:cmd
  let s:efm  = &errorformat
  call setqflist([], ' ', {'title': 'Compile: ' . l:cmd})
  let s:qfid = getqflist({'nr': '$', 'id': 0}).id
  let s:job = job_start(['/bin/sh', '-c', l:cmd], {
        \ 'cwd':     getcwd(),
        \ 'out_cb':  function('s:OnOut', [s:gen]),
        \ 'err_cb':  function('s:OnOut', [s:gen]),
        \ 'exit_cb': function('s:OnExit', [s:gen, l:cmd]),
        \ })
endfunction

function! s:CompileStop() abort
  if s:job isnot v:null && job_status(s:job) ==# 'run'
    call job_stop(s:job)
  endif
  let s:gen += 1
endfunction

command! CompileStop call <sid>CompileStop()
command! -nargs=* -bang -complete=file Compile call <sid>Compile(<bang>0, <q-args>)
