setlocal suffixesadd=.go

" gf on an import path resolves the package dir via `go list` (handles the
" @version suffix in the module cache that vim's path search can't), opens it
" in a new tab, and sets that tab's cwd there so :Grep is scoped to the
" package. Falls back to the builtin gf for plain file paths.
function! s:GoFile() abort
  let l:path = expand('<cfile>')
  let l:cwd = getcwd()
  try
    execute 'lcd' fnameescape(expand('%:p:h'))
    let l:out = systemlist(['go', 'list', '-f', '{{.Dir}}', l:path])
  finally
    execute 'lcd' fnameescape(l:cwd)
  endtry
  if v:shell_error == 0 && !empty(l:out) && isdirectory(l:out[0])
    execute 'tabedit' fnameescape(l:out[0])
    execute 'tcd' fnameescape(l:out[0])
  else
    normal! gf
  endif
endfunction
nnoremap <buffer> <silent> gf :call <SID>GoFile()<CR>

nnoremap <buffer> <localleader>b :Compile go<CR>
nnoremap <buffer> <localleader>r :Term go run %<CR>
nnoremap <buffer> <localleader>t :Compile gotest<CR>
nnoremap <buffer> <localleader>l :Compile golangci-lint<CR>
