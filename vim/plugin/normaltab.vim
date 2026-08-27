" This plugin makes Tab behave nicer in insert mode:
" - When there's a whitespace prefix, attempt to indent (or dedent with S-Tab).
" - Otherwise, try to autocomplete (tags first).

function! s:tab() abort
  if pumvisible()
    return "\<C-n>"
  endif
  if col('.') == 1 || getline('.')[:col('.')-2] =~ '^\s*$'
    return "\<Tab>"
  endif
  return "\<C-n>"
endfunction

function! s:shift_tab() abort
  if pumvisible()
    return "\<C-p>"
  endif
  if col('.') == 1 || getline('.')[:col('.')-2] =~ '^\s*$'
    return "\<C-d>"
  endif
  return "\<C-p>"
endfunction

inoremap <expr> <Tab>   <SID>tab()
inoremap <expr> <S-Tab> <SID>shift_tab()
