" Find venv python by walking up from buffer directory, checking for
" .venv/bin/python3 and venv/bin/python3, falling back to python3
function! s:find_venv_python() abort
  let l:cwd = expand('%:p:h')
  while l:cwd != '/'
    for l:venv_name in ['.venv', 'venv']
      let l:python_path = l:cwd . '/' . l:venv_name . '/bin/python3'
      if executable(l:python_path)
        return l:python_path
      endif
    endfor
    let l:cwd = fnamemodify(l:cwd, ':h')
  endwhile
  return 'python3'
endfunction

" Resolve a dotted Python import to its package/module directory via
" importlib, so gf reaches third-party packages outside the project tree
" (e.g. venv site-packages) that vim's path search can't see. Leading-dot
" relative imports return empty and fall through to the builtin gf, which
" already handles them via includeexpr.
function! jeff#python#resolve(spec) abort
  if a:spec =~# '^\.'
    return ''
  endif
  let l:code = "import importlib.util\n"
        \ . "try:\n"
        \ . "    spec = importlib.util.find_spec('" . a:spec . "')\n"
        \ . "except Exception:\n"
        \ . "    spec = None\n"
        \ . "if spec is not None:\n"
        \ . "    if spec.submodule_search_locations:\n"
        \ . "        print(list(spec.submodule_search_locations)[0])\n"
        \ . "    elif spec.origin:\n"
        \ . "        import os\n"
        \ . "        print(os.path.dirname(spec.origin))\n"
  let l:python = s:find_venv_python()
  let l:out = systemlist(join(map([l:python, '-c', l:code], {_, v -> shellescape(v)})))
  if v:shell_error == 0 && !empty(l:out)
    return l:out[0]
  endif
  return ''
endfunction
