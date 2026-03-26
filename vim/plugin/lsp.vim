" On-demand LSP support
"
" Usage:
"   :LspOn  — load plugin, configure server, start
"   :LspOff — stop the language server
"
" Requires language servers in $PATH:
"   - typescript-language-server (TypeScript/JavaScript)
"   - gopls (Go)
"   - clangd (C/C++/ObjC)
"   - nc (netcat, proxies stdio to Godot's LSP TCP server on port 6005)

function! s:LspStart() abort
  if !exists('g:loaded_lsp')
    packadd lsp
    call LspOptionsSet(#{
          \ autoComplete: v:true,
          \ showDiagOnStatusLine: v:true,
          \ })
    let s:servers = []
    if executable('typescript-language-server')
      call add(s:servers, #{
            \   name: 'typescript-language-server',
            \   filetype: ['typescript', 'typescriptreact', 'javascript', 'javascriptreact'],
            \   path: exepath('typescript-language-server'),
            \   args: ['--stdio'],
            \ })
    endif
    if executable('gopls')
      call add(s:servers, #{
            \   name: 'gopls',
            \   filetype: ['go', 'gomod'],
            \   path: exepath('gopls'),
            \   args: ['serve'],
            \ })
    endif
    if executable('clangd')
      call add(s:servers, #{
            \   name: 'clangd',
            \   filetype: ['c', 'cpp', 'objc', 'objcpp'],
            \   path: exepath('clangd'),
            \   args: [],
            \ })
    endif
    if executable('basedpyright-langserver')
      call add(s:servers, #{
            \   name: 'basedpyright',
            \   filetype: ['python'],
            \   path: exepath('basedpyright-langserver'),
            \   args: ['--stdio'],
            \ })
    endif
    if executable('nc')
      call add(s:servers, #{
            \   name: 'godot',
            \   filetype: ['gdscript'],
            \   path: exepath('nc'),
            \   args: ['127.0.0.1', '6005'],
            \ })
    endif
    call LspAddServer(s:servers)
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
