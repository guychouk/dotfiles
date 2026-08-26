" Shared gf driver for the jeff/* resolvers: a URL under the cursor opens via
" the native `open`; otherwise call a:Resolver on <cfile> - if it returns a
" directory, open it in a new tab cd'd there (so :Grep is scoped to that
" source), else fall back to the builtin gf.
function! jeff#open(Resolver) abort
  let l:spec = expand('<cfile>')
  if empty(l:spec)
    normal! gf
    return
  endif
  if l:spec =~# '^\a[[:alnum:]+.-]*://'
    call job_start(['open', l:spec])
    return
  endif
  let l:dir = a:Resolver(l:spec)
  if !empty(l:dir) && isdirectory(l:dir)
    let l:full = fnamemodify(l:dir, ':p')
    execute 'tabedit' fnameescape(l:full)
    execute 'tcd' fnameescape(l:full)
  else
    normal! gf
  endif
endfunction
