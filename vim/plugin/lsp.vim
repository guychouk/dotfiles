" On-demand LSP support
"
" Usage:
"   :LspOn  — load plugin, configure server, start
"   :LspOff — stop the language server
"
" Requires language servers in $PATH:
"   - typescript-language-server (TypeScript/JavaScript)
"   - gopls (Go)

function! s:LspStart() abort
  if !exists('g:loaded_lsp')
    packadd lsp
    call LspOptionsSet(#{
          \ autoComplete: v:true,
          \ showDiagOnStatusLine: v:true,
          \ })
    call LspAddServer([
          \ #{
          \   name: 'typescript-language-server',
          \   filetype: ['typescript', 'typescriptreact', 'javascript', 'javascriptreact'],
          \   path: exepath('typescript-language-server'),
          \   args: ['--stdio'],
          \ },
          \ #{
          \   name: 'gopls',
          \   filetype: ['go', 'gomod'],
          \   path: exepath('gopls'),
          \   args: ['serve'],
          \ },
          \ ])
    let g:loaded_lsp = 1
  endif
  execute 'LspServer start'
endfunction

function! s:OnLspAttached() abort
  setlocal keywordprg=:LspHover
  nnoremap <buffer> gd <cmd>LspGotoDefinition<CR>
  nnoremap <buffer> gr <cmd>LspShowReferences<CR>
  nnoremap <buffer> <leader>rn <cmd>LspRename<CR>
  nnoremap <buffer> [d <cmd>LspDiag prev<CR>
  nnoremap <buffer> ]d <cmd>LspDiag next<CR>
endfunction

augroup lsp_mappings
  autocmd!
  autocmd User LspAttached call <sid>OnLspAttached()
augroup END

command! -bar -nargs=0 LspOn  call <sid>LspStart()
command! -bar -nargs=0 LspOff LspServer stop
