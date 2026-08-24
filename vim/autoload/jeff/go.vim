" Resolve a Go import path to its package directory via `go list`, which
" handles the @version suffix in the module cache that vim's path search
" can't. Runs with cwd set to the buffer's directory for module/GOPATH context.
function! jeff#go#resolve(spec) abort
  let l:cwd = getcwd()
  try
    execute 'lcd' fnameescape(expand('%:p:h'))
    let l:out = systemlist(join(map(['go', 'list', '-f', '{{.Dir}}', a:spec], {_, v -> shellescape(v)})))
  finally
    execute 'lcd' fnameescape(l:cwd)
  endtry
  if v:shell_error == 0 && !empty(l:out)
    return l:out[0]
  endif
  return ''
endfunction
