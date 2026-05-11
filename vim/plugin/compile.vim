let s:job   = v:null
let s:cmd   = ''
let s:efm   = ''
let s:cwd   = ''
let s:lines = []

function! s:OnOut(ch, msg) abort
  call add(s:lines, a:msg)
  call setqflist([], 'a', {'lines': [a:msg], 'efm': s:efm})
endfunction

function! s:OnExit(job, status) abort
  call setqflist([], 'r', {
        \ 'lines': s:lines,
        \ 'efm': s:efm,
        \ 'title': printf('[%d] %s', a:status, s:cmd)
        \})
  cwindow
  echo printf('Compile: %s -> %d (%d lines)', s:cmd, a:status, len(s:lines))
endfunction

function! s:IsCompiler(name) abort
  return a:name =~# '^\S\+$' && !empty(globpath(&runtimepath, 'compiler/' . a:name . '.vim'))
endfunction

function! s:Compile(bang, arg) abort
  let l:arg = trim(a:arg)
  if empty(l:arg)
    let l:cmd = substitute(&makeprg, '\s*\$\*', '', 'g')
  elseif !a:bang && s:IsCompiler(l:arg)
    execute 'compiler' l:arg
    let l:cmd = substitute(&makeprg, '\s*\$\*', '', 'g')
  else
    let l:cmd = l:arg
  endif
  if empty(l:cmd)
    echohl WarningMsg
    echo 'no command'
    echohl None
    return
  endif
  if s:job isnot v:null && job_status(s:job) ==# 'run'
    call job_stop(s:job)
  endif
  let s:cmd   = l:cmd
  let s:efm   = &errorformat
  let s:cwd   = getcwd()
  let s:lines = []
  call setqflist([], 'r', {'title': 'Compile: ' . l:cmd})
  let s:job = job_start(['/bin/sh', '-c', l:cmd], {
        \ 'cwd':     s:cwd,
        \ 'out_cb':  function('s:OnOut'),
        \ 'err_cb':  function('s:OnOut'),
        \ 'exit_cb': function('s:OnExit'),
        \ })
endfunction

function! s:CompileStop() abort
  if s:job isnot v:null && job_status(s:job) ==# 'run'
    call job_stop(s:job)
  endif
endfunction

function! s:CompileDispatch(bang, arg) abort
  if a:bang
    if empty(s:cmd)
      echohl WarningMsg
      echo 'nothing to recompile'
      echohl None
      return
    endif
    call s:Compile(1, s:cmd)
  else
    call s:Compile(0, a:arg)
  endif
endfunction

command! -nargs=* -bang -complete=compiler Compile     call <sid>CompileDispatch(<bang>0, <q-args>)
command!                                   CompileStop call <sid>CompileStop()
