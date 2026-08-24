" Resolve a bare JS/TS import specifier to its node_modules package
" directory, searching upward from the file. Relative/absolute specifiers
" return empty so they fall through to the builtin gf, which uses the
" buffer's suffixesadd/includeexpr.
function! jeff#js#resolve(spec) abort
  if a:spec =~# '^[./]'
    return ''
  endif
  let l:parts = split(a:spec, '/')
  let l:pkg = a:spec[0] ==# '@' ? join(l:parts[0:1], '/') : l:parts[0]
  return finddir('node_modules/' . l:pkg, expand('%:p:h') . ';')
endfunction
