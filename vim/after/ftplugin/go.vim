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

" :GoSrc opens a new tab cd'd to the standard library source ($GOROOT/src),
" so :Grep there searches the stdlib. Built per-buffer since GOROOT tracks
" the Go version mise selects for the current project.
function! s:GoSrc() abort
  let l:root = trim(system('go env GOROOT'))
  if v:shell_error != 0 || empty(l:root)
    echohl WarningMsg | echo 'go env GOROOT failed' | echohl None
    return
  endif
  execute 'tabedit' fnameescape(l:root . '/src')
  execute 'tcd' fnameescape(l:root . '/src')
endfunction
command! -buffer GoSrc call <SID>GoSrc()

" Override the distributed :GoKeywordPrg (still bound to K via keywordprg) to
" render `go doc` into a reusable scratch window instead of the blocking pager.
" iskeyword is widened so the word under the cursor includes the package
" selector (fmt.Println, not just Println); -C anchors go doc in the file's
" package so imports and same-package names resolve.
function! s:GoDoc() abort
  let l:isk = &l:iskeyword
  setlocal iskeyword+=.
  let l:word = expand('<cword>')
  let &l:iskeyword = l:isk
  let l:out = systemlist(['go', 'doc', '-C', expand('%:p:h'), l:word])
  if v:shell_error != 0
    echohl WarningMsg | echo 'go doc: ' . join(l:out, ' ') | echohl None
    return
  endif
  for l:w in range(1, winnr('$'))
    if getbufvar(winbufnr(l:w), 'godoc_scratch', 0)
      execute l:w . 'wincmd w' | break
    endif
  endfor
  if !get(b:, 'godoc_scratch', 0)
    botright new
    let b:godoc_scratch = 1
    setlocal buftype=nofile bufhidden=hide noswapfile nobuflisted winfixheight
    setlocal syntax=go
    nnoremap <buffer> <silent> q <C-w>c
  endif
  setlocal modifiable
  silent %delete _
  call setline(1, l:out)
  setlocal nomodifiable
  execute 'resize' min([max([len(l:out), 3]), 20])
  normal! gg
endfunction
command! -buffer -nargs=* GoKeywordPrg call <SID>GoDoc()

nnoremap <buffer> <localleader>b :Compile go<CR>
nnoremap <buffer> <localleader>r :Term go run %<CR>
nnoremap <buffer> <localleader>t :Compile gotest<CR>
nnoremap <buffer> <localleader>l :Compile golangci-lint<CR>
