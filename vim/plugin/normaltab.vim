" This plugin makes Tab behave better in insert mode.
" When there's a whitespace prefix, it attempts to indent (or dedent with S-Tab).
" Otherwise, we try to autocomplete: omni when LSP is active, keyword (tags first) as fallback.

function! s:Tab() abort
  if pumvisible()
    return "\<C-n>"
  endif
  if col('.') == 1 || getline('.')[:col('.')-2] =~ '^\s*$'
    return "\<Tab>"
  endif
  return exists('g:loaded_lsp') ? "\<C-x>\<C-o>" : "\<C-n>"
endfunction

function! s:STab() abort
  if pumvisible()
    return "\<C-p>"
  endif
  if col('.') == 1 || getline('.')[:col('.')-2] =~ '^\s*$'
    return "\<C-d>"
  endif
  return exists('g:loaded_lsp') ? "\<C-x>\<C-o>" : "\<C-p>"
endfunction

inoremap <expr> <Tab>   <SID>Tab()
inoremap <expr> <S-Tab> <SID>STab()
