let s:job  = v:null
let s:gen  = 0
let s:qfid = 0
let s:cmd  = ''
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
    let l:head = matchstr(l:arg, '^\S\+')
    if !a:bang && s:IsCompiler(l:head)
      execute 'compiler' l:head
    endif
    let l:cmd = l:arg
  endif
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

function! s:CompileDispatch(bang, arg) abort
  if a:bang
    if empty(s:cmd)
      echohl WarningMsg | echo 'nothing to recompile' | echohl None
      return
    endif
    let l:saved_efm = &errorformat
    let &errorformat = s:efm
    try
      call s:Compile(1, s:cmd)
    finally
      let &errorformat = l:saved_efm
    endtry
  else
    call s:Compile(0, a:arg)
  endif
endfunction

function! s:CompleteCompile(arg, line, pos) abort
  if a:arg =~# '^\(\.*/\|/\|\~\)'
    return join(getcompletion(a:arg, 'file'), "\n")
  endif
  return join(getcompletion(a:arg, 'compiler'), "\n")
endfunction

command! -nargs=* -bang -complete=custom,<sid>CompleteCompile Compile call <sid>CompileDispatch(<bang>0, <q-args>)
command!                                                       CompileStop call <sid>CompileStop()
