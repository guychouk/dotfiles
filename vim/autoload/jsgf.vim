" gf for JS/TS: resolve a bare import specifier to its node_modules package
" directory (searching upward from the file) and open it in a new tab with the
" cwd set there, mirroring the Go gf. Relative/absolute specifiers fall through
" to the builtin gf, which uses the buffer's suffixesadd/includeexpr.
function! jsgf#open() abort
  let l:spec = expand('<cfile>')
  if empty(l:spec) || l:spec =~# '^[./]'
    normal! gf
    return
  endif
  let l:parts = split(l:spec, '/')
  let l:pkg = l:spec[0] ==# '@' ? join(l:parts[0:1], '/') : l:parts[0]
  let l:dir = finddir('node_modules/' . l:pkg, expand('%:p:h') . ';')
  if !empty(l:dir) && isdirectory(l:dir)
    let l:full = fnamemodify(l:dir, ':p')
    execute 'tabedit' fnameescape(l:full)
    execute 'tcd' fnameescape(l:full)
  else
    normal! gf
  endif
endfunction
