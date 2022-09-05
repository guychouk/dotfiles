"" Settings

set mouse=a                                                " Enable scrolling
set clipboard=unnamedplus                                  " Enables OS clipboard support
set completeopt-=preview                                   " Disable preview for completion selection
set encoding=utf-8                                         " Encoding for files
set foldmethod=syntax                                      " Set foldmethod to syntax
set history=1000                                           " Set command history to 1000
set ignorecase                                             " Ignore case of characters in search patterns
set laststatus=0                                           " Don't show statusline by default
set list                                                   " Show whitespace characters
set listchars=tab:┊·,trail:·,extends:>,precedes:<,nbsp:+   " Set default whitespace characters
set nohlsearch                                             " Disable search highlighting
set noshowmode                                             " Don't show current mode in last line
set noshowcmd                                              " Don't show information relating to current mode
set noswapfile                                             " No swap files
set nowrap                                                 " Disable line wrapping
set noruler                                                " Disable ruler
set nonumber                                               " Don't show current line number and relative line numbers
set norelativenumber                                       " Don't show relative line numbers
set scrolloff=1                                            " Minimum number of lines above and below cursor
set shortmess+=I                                           " Suppress intro message
set shortmess+=c                                           " Suppress ins-completion-menu messages
set signcolumn=yes                                         " Always show signcolumn
set smartcase                                              " Override 'ignorecase' if search pattern contains uppercase characters
set updatetime=300                                         " Set CursorHold delay time

"" Colors

set termguicolors
let g:tokyonight_style = 'night'
let g:tokyonight_enable_italic = 0
let g:tokyonight_disable_italic_comment = 1
colorscheme tokyonight

hi Normal              guibg=none
hi LineNr              guibg=none
hi SignColumn          guibg=none

hi DiagnosticSignError guibg=none guifg=red
hi DiagnosticSignWarn  guibg=none guifg=orange
hi DiagnosticSignHint  guibg=none guifg=gray
hi DiagnosticSignInfo  guibg=none guifg=cyan

"" Globals

let g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.6, 'relative': v:true } }
let g:fzf_preview_window = ['down:50%:hidden', 'ctrl-]']

let g:rooter_silent_chdir = 1
let g:rooter_patterns = ['.git', 'Makefile', 'package.json', 'init.vim', '.envrc']
let g:rooter_change_directory_for_non_project_files = 'current'

let g:vsnip_filetypes = {
			\ 'typescript': ['javascript'],
			\ 'javascriptreact': ['javascript'],
			\ 'typescriptreact': ['javascript'],
			\}
let g:vsnip_snippet_dir = system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/snippets"')

let g:indentLine_fileType = ['yaml', 'yml']

let g:gh_repo_map = '<leader><leader>go'
let g:gh_line_map_default = 0
let g:gh_line_blame_map_default = 0
let g:gh_open_command = 'fn() { echo "$@" | pbcopy; }; fn '

let g:user_emmet_install_global = 0

"" Functions

function! s:tab_completion()
	let line = getline('.')
	let substr = strpart(line, 0, col('.') - 1)
	let is_preceded_by_dot = match(substr, '\.') != -1
	let is_cursor_on_first_column = col('.') == 1
	let has_preceding_whitespace = line[col('.') - 2]  =~# '\s'
	if (is_cursor_on_first_column || has_preceding_whitespace)
		return "\<Tab>"
	elseif (is_preceded_by_dot)
		return "\<C-X>\<C-O>"
	elseif (vsnip#expandable())
		return "\<C-j>"
	else
		return "\<C-N>"
	endif
endfunction

function! s:zoom_pane_toggle()
	if exists('t:zoomed') && t:zoomed
		execute t:zoom_winrestcmd
		let t:zoomed = 0
	else
		let t:zoom_winrestcmd = winrestcmd()
		resize
		vertical resize
		let t:zoomed = 1
	endif
endfunction

function! s:vim_help_cword()
	if (index(['vim','help'], &filetype) >= 0)
		exe 'h' expand('<cword>')
	endif
endfunction

function! s:search_range(type, ...)
	let sel_save = &selection
	let &selection = 'inclusive'
	let reg_save = @@
	if a:0
		silent exe 'normal! gvy'
	elseif a:type == 'line'
		silent exe "normal! '[V']y"
	else
		silent exe "normal! `[v`]y"
	endif
	exe 'Rg' @@
	let &selection = sel_save
	let @@ = reg_save
endfunction

function! s:remove_qf_item()
  let l:qf_list = getqflist()
  if len(l:qf_list) <= 1
	  cclose
	  return
  endif
  let l:curqfidx = line('.') - 1
  call remove(l:qf_list, curqfidx)
  call setqflist(l:qf_list, 'r')
  execute l:curqfidx + 1 . 'cfirst'
  copen
endfunction

"" Mappings

let mapleader = "\<Space>"
let maplocalleader = "\<Space>"

" Break undo before deleting a word
inoremap <C-W> <C-G>u<C-W>
" Break undo before deleting a line
inoremap <C-U> <C-G>u<C-U>

nmap          R              :%s//g<Left><Left>
nmap <silent> K              :call <SID>vim_help_cword()<CR>
nmap <silent> yoz            :call <SID>zoom_pane_toggle()<CR>
nmap          <leader>/      :Rg ""<Left>
nmap <silent> <leader>.      :GFiles<CR>
nmap <silent> <leader>b      :Buffers<CR>
nmap <silent> <leader>g      :Git<CR>
nmap <silent> <leader>j      :sp<CR>
nmap <silent> <leader>l      :vsp<CR>
nmap <silent> <leader>o      :Files<CR>
nmap <silent> <leader>p      :Commands<CR>
nmap <silent> <leader>q      :q<CR>
nmap <silent> <leader>s      <plug>(statusline_toggle)
nmap <silent> <leader>t      :BTags<CR>
nmap <silent> <leader>w      :w<CR>
nmap <silent> <leader>ec     :e ~/.config/nvim/init.vim<CR>
nmap <silent> <leader>=h     :exe "resize +5"<CR>
nmap <silent> <leader>-h     :exe "resize -5"<CR>
nmap <silent> <leader>=v     :exe "vertical resize +5"<CR>
nmap <silent> <leader>-v     :exe "vertical resize -5"<CR>

xmap <silent> <leader>ea     <plug>(EasyAlign)
xmap <silent> <leader>/      :<c-u>call <SID>search_range(visualmode(), 1)<CR>

nmap <silent> <leader><tab>  <plug>(fzf-maps-n)
xmap <silent> <leader><tab>  <plug>(fzf-maps-x)
omap <silent> <leader><tab>  <plug>(fzf-maps-o)

imap <silent> <c-x><c-k>     <plug>(fzf-complete-word)
imap <silent> <c-x><c-l>     <plug>(fzf-complete-line)
imap <silent> <c-x><c-f>     <plug>(fzf-complete-path)
imap <silent> <c-x><c-x>     <plug>(custom-fzf-complete-snippet)

imap <silent> <expr> <C-j>   vsnip#expandable() ? '<plug>(vsnip-expand)'    : '<C-j>'
smap <silent> <expr> <C-j>   vsnip#expandable() ? '<plug>(vsnip-expand)'    : '<C-j>'

imap <silent> <expr> <Tab>   vsnip#jumpable(1)  ? '<plug>(vsnip-jump-next)' : (pumvisible() ? '<C-n>' : <SID>tab_completion())
smap <silent> <expr> <Tab>   vsnip#jumpable(1)  ? '<plug>(vsnip-jump-next)' : (pumvisible() ? '<C-n>' : <SID>tab_completion())
imap <silent> <expr> <S-Tab> vsnip#jumpable(-1) ? '<plug>(vsnip-jump-prev)' : (pumvisible() ? '<C-p>' : '<S-Tab>')
smap <silent> <expr> <S-Tab> vsnip#jumpable(-1) ? '<plug>(vsnip-jump-prev)' : (pumvisible() ? '<C-p>' : '<S-Tab>')

"" LSP
lua <<EOF
	local signs = {
		{ name = "DiagnosticSignError", text = "•" },
		{ name = "DiagnosticSignWarn",  text = "•" },
		{ name = "DiagnosticSignHint",  text = "•" },
		{ name = "DiagnosticSignInfo",  text = "•" },
	}
	for _, sign in ipairs(signs) do
		vim.fn.sign_define(sign.name, {
			texthl = sign.name,
			text = sign.text,
			numhl = ""
		})
	end
	local opts = { noremap=true, silent=true }
	vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
	vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
	vim.keymap.set('n', '<leader>d', vim.diagnostic.setloclist, opts)
	local on_attach = function(client, bufnr)
		vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
		local bufopts = { noremap=true, silent=true, buffer=bufnr }
		vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
		vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
		vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
		vim.keymap.set('n', '<leader>f', vim.lsp.buf.formatting, bufopts)
		vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, bufopts)
	end
	local lsp_flags = {
		debounce_text_changes = 100,
	}
	require('lspconfig').tsserver.setup {
		on_attach = on_attach,
		flags = lsp_flags,
		handlers = {
			["textDocument/publishDiagnostics"] = vim.lsp.with(
				vim.lsp.diagnostic.on_publish_diagnostics, {
					virtual_text=false
				}
			),
		}
	}
	-- Treesitter
	require('nvim-treesitter.configs').setup {
	ensure_installed = { "javascript", "typescript" },
	sync_install = false,
	auto_install = true,
	highlight = {
		enable = true,
		-- Setting this to true will run `:h syntax` and tree-sitter at the same time.
		-- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
		-- Using this option may slow down your editor, and you may see some duplicate highlights.
		-- Instead of true it can also be a list of languages
		additional_vim_regex_highlighting = false,
		},
	}
EOF

"" Filetype Settings
autocmd FileType html,css
			\ setlocal shiftwidth=2
			\| EmmetInstall
autocmd FileType c
			\ setlocal tabstop=4
			\| setlocal shiftwidth=4
autocmd FileType json
			\ setlocal expandtab
			\| setlocal tabstop=2
			\| setlocal shiftwidth=2
			\| setlocal foldlevel=1
autocmd FileType javascript,javascriptreact
			\ setlocal expandtab
			\| setlocal tabstop=2
			\| setlocal shiftwidth=2
			\| setlocal foldlevel=99
			\| setlocal makeprg=./node_modules/.bin/eslint
			\| setlocal errorformat=%f:\ line\ %l\\,\ col\ %c\\,\ %m,%-G%.%#
autocmd FileType typescript,typescriptreact
			\ setlocal expandtab
			\| setlocal tabstop=2
			\| setlocal shiftwidth=2
			\| setlocal foldlevel=99
			\| setlocal makeprg=./node_modules/.bin/tsc
			\| setlocal errorformat=%f\ %#(%l\\,%c):\ %trror\ TS%n:\ %m,%trror\ TS%n:\ %m,%-G%.%#
autocmd FileType repl
			\ vmap <buffer> <silent> <enter> <plug>(VimuxSlime)
			\| nmap <buffer> <silent> <enter> <plug>(VimuxSlimeLine)
autocmd FileType gitcommit,gitrebase,gitconfig set bufhidden=delete
autocmd BufNewFile,BufRead init.vim let g:gitgutter_git_args='--git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
autocmd CursorMoved * :call v:lua.require'diagnostics'.echo_diagnostic()
autocmd FileType qf map <buffer> dd :RemoveQFItem<cr>

autocmd! FileType fzf
autocmd  FileType fzf set nonumber norelativenumber

"" Commands

command! Gqf GitGutterQuickFix | copen
command! RemoveQFItem :call <SID>remove_qf_item()

"" Netrw

let g:netrw_localcopydircmd = 'cp -r'

hi! link netrwMarkFile Search

function! NetrwMapping()
	" VinegarUp using h
	nmap <buffer> h -
	" l to open
	nmap <buffer> l <CR>
	" close netrw and return to previous buffer
	nmap <buffer> <ESC> <C-^>
	" <TAB> marks a file
	nmap <buffer> <TAB> mf
	" <S-TAB> unmarks a file
	nmap <buffer> <S-TAB> mF
	" Clear all marks
	nmap <buffer> <Leader><TAB> mu
	" new file
	nmap <buffer> ff %:w<CR>:buffer #<CR>
	" rename / move
	nmap <buffer> fr R
	" set target
	nmap <buffer> ft mt
	" copy to target
	nmap <buffer> fc mc
	" set target and copy
	nmap <buffer> fC mtmc
	" move to target
	nmap <buffer> fx mm
	" set target and move
	nmap <buffer> fX mtmm
endfunction

augroup netrw_mapping
	autocmd!
	autocmd filetype netrw call NetrwMapping()
augroup END
