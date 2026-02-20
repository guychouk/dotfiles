" normaltab.vim - Tab that behaves like a normal tab
"
" On whitespace-only prefix: indent/dedent
" Otherwise: complete (omni if available, keyword fallback)

function! s:Tab() abort
  if pumvisible()
    return "\<C-n>"
  endif
  if col('.') == 1 || getline('.')[:col('.')-2] =~ '^\s*$'
    return "\<Tab>"
  endif
  return &omnifunc != '' ? "\<C-x>\<C-o>" : "\<C-n>"
endfunction

function! s:STab() abort
  if pumvisible()
    return "\<C-p>"
  endif
  if col('.') == 1 || getline('.')[:col('.')-2] =~ '^\s*$'
    return "\<C-d>"
  endif
  return &omnifunc != '' ? "\<C-x>\<C-o>" : "\<C-p>"
endfunction

inoremap <expr> <Tab>   <SID>Tab()
inoremap <expr> <S-Tab> <SID>STab()
