"" Settings

let &clipboard     .= "unnamedplus"
let &completeopt    = "menu,preview"
let &hlsearch       = 0
let &laststatus     = 0
let &list           = 1
let &listchars      = 'tab:┊·,trail:·,extends:>,precedes:<,nbsp:+'
let &mouse          = "a"
let &ruler          = 0
let &scrolloff      = 1
let &shortmess     .= "I"
let &shortmess     .= "c"
let &showcmd        = 0
let &showmode       = 0
let &signcolumn     = "yes"
let &smartcase      = 1
let &swapfile       = 0
let &termguicolors  = 1
let &updatetime     = 300

"" Colors

colorscheme tokyonight-night

hi Normal              guibg=none
hi LineNr              guibg=none
hi SignColumn          guibg=none
hi EndOfBuffer         guibg=none guifg=gray

"" Plugin Settings

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

function! s:get_visual_selection()
	let [line_start, column_start] = getpos("'<")[1:2]
	let [line_end, column_end] = getpos("'>")[1:2]
	let lines = getline(line_start, line_end)
	if len(lines) == 0
		return ''
	endif
	let lines[-1] = lines[-1][: column_end - (&selection == 'inclusive' ? 1 : 2)]
	let lines[0] = lines[0][column_start - 1:]
	return lines
endfunction

function! s:fzf_complete_snippet(...)
  return fzf#vim#complete(fzf#wrap({
  \ 'source':  "cat " . $HOME . "/.config/nvim/snippets/" . &ft . ".json" . " | jq -r 'to_entries[] | \"\\(.key): \\(.value.prefix)\"'",
  \ 'reducer': { lines -> trim(split(lines[0], ':')[1]) } }))
endfunction

function! s:vimux_slime_selection()
	let lines = s:get_visual_selection()
	for line in lines
		sleep 50m
		call VimuxRunCommand(line)
	endfor
endfunction

function! s:vimux_slime_line()
	let rv = getreg('"')
	let rt = getregtype('"')
	execute "normal! yy"
	call VimuxRunCommand(trim(@"))
	call setreg('"', rv, rt)
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
nmap <silent> <leader>w      :w<CR>
nmap <silent> <leader>tb     :BTags<CR>
nmap <silent> <leader>tt     :Tags<CR>
nmap <silent> <leader>ec     :e ~/.config/nvim/init.vim<CR>

nmap <silent> <leader>=h     :exe "resize +5"<CR>
nmap <silent> <leader>-h     :exe "resize -5"<CR>
nmap <silent> <leader>=v     :exe "vertical resize +5"<CR>
nmap <silent> <leader>-v     :exe "vertical resize -5"<CR>

xmap <silent> <leader>ea     <plug>(EasyAlign)
xmap <silent> <leader>ys     <plug>(VSurround)
xmap <silent> <leader>/      :<c-u>call <SID>search_range(visualmode(), 1)<CR>

nmap <silent> <leader><tab>  <plug>(fzf-maps-n)
xmap <silent> <leader><tab>  <plug>(fzf-maps-x)
omap <silent> <leader><tab>  <plug>(fzf-maps-o)

imap <silent>        <c-x><c-k>     <plug>(fzf-complete-word)
imap <silent>        <c-x><c-l>     <plug>(fzf-complete-line)
imap <silent>        <c-x><c-f>     <plug>(fzf-complete-path)

imap <silent> <expr> <c-x><c-x>     <SID>fzf_complete_snippet()

imap <silent> <expr> <C-j>   vsnip#expandable() ? '<plug>(vsnip-expand)'    : '<C-j>'
smap <silent> <expr> <C-j>   vsnip#expandable() ? '<plug>(vsnip-expand)'    : '<C-j>'

imap <silent> <expr> <Tab>   vsnip#jumpable(1)  ? '<plug>(vsnip-jump-next)' : (pumvisible() ? '<C-n>' : <SID>tab_completion())
smap <silent> <expr> <Tab>   vsnip#jumpable(1)  ? '<plug>(vsnip-jump-next)' : (pumvisible() ? '<C-n>' : <SID>tab_completion())
imap <silent> <expr> <S-Tab> vsnip#jumpable(-1) ? '<plug>(vsnip-jump-prev)' : (pumvisible() ? '<C-p>' : '<S-Tab>')
smap <silent> <expr> <S-Tab> vsnip#jumpable(-1) ? '<plug>(vsnip-jump-prev)' : (pumvisible() ? '<C-p>' : '<S-Tab>')

"" Filetype Settings

autocmd FileType markdown
			\  setlocal shiftwidth=2
			\| setlocal foldlevel=99

autocmd FileType html,css
			\  setlocal shiftwidth=2
			\| EmmetInstall

autocmd FileType c,cpp
			\  setlocal tabstop=4
			\| setlocal shiftwidth=4
			\| setlocal foldlevel=99

autocmd FileType json
			\  setlocal expandtab
			\| setlocal tabstop=2
			\| setlocal shiftwidth=2
			\| setlocal foldlevel=1

autocmd FileType yaml
			\ setlocal foldlevel=3

autocmd FileType javascript,javascriptreact
			\  setlocal expandtab
			\| setlocal tabstop=4
			\| setlocal shiftwidth=4
			\| setlocal foldlevel=99
			\| setlocal makeprg=./node_modules/.bin/eslint
			\| setlocal errorformat=%f:\ line\ %l\\,\ col\ %c\\,\ %m,%-G%.%#
			\| EmmetInstall

autocmd FileType typescript,typescriptreact
			\  setlocal expandtab
			\| setlocal tabstop=4
			\| setlocal shiftwidth=4
			\| setlocal foldlevel=99
			\| setlocal makeprg=./node_modules/.bin/tsc
			\| setlocal errorformat=%f\ %#(%l\\,%c):\ %trror\ TS%n:\ %m,%trror\ TS%n:\ %m,%-G%.%#
			\| EmmetInstall

autocmd FileType repl
			\  setlocal filetype=bash
			\| nmap <buffer> <silent> <enter> :call <SID>vimux_slime_line()<CR>
			\| vmap <buffer> <silent> <enter> :<c-u>call <SID>vimux_slime_selection()<CR>

autocmd FileType netrw
			\  nmap <buffer> h -
			\| nmap <buffer> l <CR>
			\| nmap <buffer> <ESC> <C-^>
			\| nmap <buffer> ff %:w<CR>:buffer #<CR>

autocmd FileType qf map <buffer> dd :RemoveQFItem<cr>

autocmd FileType gitcommit,gitrebase,gitconfig set bufhidden=delete

autocmd BufNewFile,BufRead init.vim let g:gitgutter_git_args='--git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

"" Commands

command! Gqf GitGutterQuickFix | copen
command! RemoveQFItem call <SID>remove_qf_item()
command! Qbuffers call setqflist(map(filter(range(1, bufnr('$')), 'buflisted(v:val)'), '{"bufnr":v:val}')) | copen

" Exclude file names from Rg matches
command! -bang -nargs=* Rg call fzf#vim#grep("rg --column --line-number --no-heading --color=always --smart-case ".<q-args>, 1, {'options': '--delimiter : --nth 4..'}, <bang>0)

"" Treesitter

lua <<EOF
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"

require('nvim-treesitter.configs').setup {
	ensure_installed = { "javascript", "typescript" },
	sync_install = false,
	auto_install = true,
	highlight = {
		enable = true,
		additional_vim_regex_highlighting = false,
	},
}
EOF
