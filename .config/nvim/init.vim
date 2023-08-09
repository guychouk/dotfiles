"" Settings

let &clipboard     .= "unnamedplus"
let &cursorline     = 1
let &completeopt    = "menu,preview"
let &hlsearch       = 0
let &laststatus     = 0
let &list           = 1
let &listchars      = 'tab:⋮ ,trail:·,extends:>,precedes:<,nbsp:+'
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
let &foldmethod     = "expr"
let &foldexpr       = "nvim_treesitter#foldexpr()"
let &fillchars      = 'eob: '

"" Colors

colorscheme catppuccin-mocha

"" Plugins

" Add Homebrew installed fzf to runtimepath
set rtp+=/usr/local/opt/fzf

let g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.7, 'relative': v:true } }
let g:fzf_preview_window = ['down:70%:hidden', 'ctrl-]']

let g:rooter_silent_chdir = 1
let g:rooter_patterns = ['.git', 'Makefile', 'package.json', 'init.vim', '.envrc']
let g:rooter_change_directory_for_non_project_files = 'current'

let g:vsnip_filetypes = {
			\ 'typescript': ['javascript'],
			\ 'javascriptreact': ['javascript'],
			\ 'typescriptreact': ['javascript'],
			\}
let g:vsnip_snippet_dir = system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/snippets"')

let g:gh_repo_map = '<leader><leader>go'
let g:gh_line_map_default = 0
let g:gh_line_blame_map_default = 0
let g:gh_open_command = 'fn() { echo "$@" | pbcopy; }; fn '

let g:user_emmet_install_global = 0

let g:indentLine_fileType = ['yaml', 'yml']

let g:goyo_width = 120

"" Functions

function! s:insert_post_link(file)
  if len(a:file) != 0
    let ext = expand('%:e')
    let new_ext = '.html'
    let modified_file = substitute(substitute(a:file[0], '\.'.ext.'$', new_ext, ''), '\.', '', '')
    return modified_file
  endif
endfunction

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

function! s:toggle_pane_zoom()
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
	call VimuxRunCommand(@v)
endfunction

function! s:vimux_slime_line()
	let rv = getreg('"')
	let rt = getregtype('"')
	execute "normal! yy"
	call VimuxRunCommand(trim(@"))
	call setreg('"', rv, rt)
endfunction

function! s:goyo_enter()
	silent !tmux set status off
	silent !tmux list-panes -F '\#F' | grep -q Z || tmux resize-pane -Z
endfunction

function! s:goyo_leave()
	silent !tmux set status on
	silent !tmux list-panes -F '\#F' | grep -q Z && tmux resize-pane -Z
endfunction

"" Mappings

let mapleader = "\<Space>"

" Break undo before deleting a word
inoremap <C-W> <C-G>u<C-W>

" Break undo before deleting a line
inoremap <C-U> <C-G>u<C-U>

nmap          R                     :%s//g<Left><Left>
nmap          <leader>/             :Rg -g '!tags' ""<Left>
nmap <silent> K                     :call <SID>vim_help_cword()<CR>
nmap <silent> yoz                   :call <SID>toggle_pane_zoom()<CR>
nmap <silent> <F10>                 :Goyo<CR>
nmap <silent> <leader>.             :GFiles<CR>
nmap <silent> <leader>b             :Buffers<CR>
nmap <silent> <leader>g             :Git<CR>
nmap <silent> <leader>j             :sp<CR>
nmap <silent> <leader>l             :vsp<CR>
nmap <silent> <leader>o             :Files<CR>
nmap <silent> <leader>p             :Commands<CR>
nmap <silent> <leader>q             :q<CR>
nmap <silent> <leader>s             <plug>(statusline_toggle)
nmap <silent> <leader>w             :w<CR>
nmap <silent> <leader>tb            :BTags<CR>
nmap <silent> <leader>tt            :Tags<CR>
nmap <silent> <leader>ec            :e ~/.config/nvim/init.vim<CR>
nmap <silent> <leader>=h            :exe "resize +5"<CR>
nmap <silent> <leader>-h            :exe "resize -5"<CR>
nmap <silent> <leader>=v            :exe "vertical resize +5"<CR>
nmap <silent> <leader>-v            :exe "vertical resize -5"<CR>
nmap <silent> <leader><tab>         <plug>(fzf-maps-n)
nmap <silent> <leader><enter>       :call <SID>vimux_slime_line()<CR>

omap <silent>        <leader><tab>  <plug>(fzf-maps-o)

xmap <silent>        <leader>ea     <plug>(EasyAlign)
xmap <silent>        <leader>ys     <plug>(VSurround)
xmap <silent>        <leader><tab>  <plug>(fzf-maps-x)

imap <silent>        <c-x><c-k>     <plug>(fzf-complete-word)
imap <silent>        <c-x><c-l>     <plug>(fzf-complete-line)
imap <silent>        <c-x><c-f>     <plug>(fzf-complete-path)
imap <silent> <expr> <c-x><c-x>     <SID>fzf_complete_snippet()
imap <silent> <expr> <c-x><c-o>     fzf#vim#complete({
					\ 'source':  'find ./notes -name "*.md"',
					\ 'reducer': function('<sid>insert_post_link'),
					\ 'options': '--reverse',
					\ 'window': { 'width': 0.4, 'height': 0.7 }
					\ })

" Snippets and Tab completion

imap <silent> <expr> <Tab>          vsnip#jumpable(1)  ? '<plug>(vsnip-jump-next)' : (pumvisible() ? '<C-n>' : <SID>tab_completion())
imap <silent> <expr> <S-Tab>        vsnip#jumpable(-1) ? '<plug>(vsnip-jump-prev)' : (pumvisible() ? '<C-p>' : '<S-Tab>')
imap <silent> <expr> <C-j>          vsnip#expandable() ? '<plug>(vsnip-expand)'    : '<C-j>'

smap <silent> <expr> <C-j>          vsnip#expandable() ? '<plug>(vsnip-expand)'    : '<C-j>'
smap <silent> <expr> <Tab>          vsnip#jumpable(1)  ? '<plug>(vsnip-jump-next)' : (pumvisible() ? '<C-n>' : <SID>tab_completion())
smap <silent> <expr> <S-Tab>        vsnip#jumpable(-1) ? '<plug>(vsnip-jump-prev)' : (pumvisible() ? '<C-p>' : '<S-Tab>')

"" Filetype Settings

autocmd FileType markdown
			\  setlocal shiftwidth=2
			\| setlocal foldlevel=99

autocmd FileType html,css
			\  setlocal shiftwidth=2
			\| setlocal foldlevel=99
			\| EmmetInstall

autocmd FileType c,cpp
			\  setlocal tabstop=4
			\| setlocal shiftwidth=4
			\| setlocal foldlevel=99

autocmd FileType json
			\  setlocal expandtab
			\| setlocal tabstop=2
			\| setlocal shiftwidth=2
			\| setlocal foldlevel=2

autocmd FileType yaml
			\ setlocal foldlevel=4

autocmd FileType javascript,javascriptreact
			\  setlocal expandtab
			\| setlocal tabstop=2
			\| setlocal shiftwidth=2
			\| setlocal foldlevel=99
			\| setlocal makeprg=eslint
			\| setlocal errorformat=%f:\ line\ %l\\,\ col\ %c\\,\ %m,%-G%.%#
			\| let maplocalleader="\\"
			\| map <localleader>r :call VimuxRunCommand("node " . bufname("%"))<CR>
			\| map <localleader>\ :VimuxRunLastCommand<CR>
			\| EmmetInstall

autocmd FileType typescript,typescriptreact
			\  setlocal expandtab
			\| setlocal tabstop=4
			\| setlocal shiftwidth=4
			\| setlocal foldlevel=99
			\| setlocal makeprg=tsc
			\| setlocal errorformat=%f\ %#(%l\\,%c):\ %trror\ TS%n:\ %m,%trror\ TS%n:\ %m,%-G%.%#
			\| let maplocalleader="\\"
			\| map <localleader>r :call VimuxRunCommand("ts-node " . bufname("%"))<CR>
			\| map <localleader>\ :VimuxRunLastCommand<CR>
			\| map <localleader>e :Dispatch! -compiler=eslint eslint --ext "ts" --format compact .<CR>
			\| EmmetInstall

autocmd FileType repl
			\  setlocal filetype=bash
			\| nmap <buffer> <silent> <enter> :call <SID>vimux_slime_line()<CR>
			\| vmap <buffer> <silent> <enter> "vy :call <SID>vimux_slime_selection()<CR>

autocmd FileType netrw
			\  nmap <buffer> h -
			\| nmap <buffer> l <CR>
			\| nmap <buffer> <ESC> <C-^>
			\| nmap <buffer> ff %:w<CR>:buffer #<CR>

autocmd FileType qf
			\ map <buffer> dd :RemoveQFItem<cr>

autocmd FileType gitcommit,gitrebase,gitconfig
			\ set bufhidden=delete

autocmd BufNewFile,BufRead init.vim
			\ let g:gitgutter_git_args='--git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

autocmd! User GoyoEnter nested call <SID>goyo_enter()

autocmd! User GoyoLeave nested call <SID>goyo_leave()

"" Commands

command! Gqf GitGutterQuickFix | copen
command! RemoveQFItem call <SID>remove_qf_item()
command! Qbuffers call setqflist(map(filter(range(1, bufnr('$')), 'buflisted(v:val)'), '{"bufnr":v:val}')) | copen

" Exclude file names from Rg matches
command! -bang -nargs=* Rg call fzf#vim#grep("rg --column --line-number --no-heading --color=always --smart-case ".<q-args>, 1, {'options': '--delimiter : --nth 4..'}, <bang>0)

" Triger `autoread` when files change on disk
autocmd FileChangedShellPost * echohl WarningMsg | echo "File changed on disk. Buffer reloaded." | echohl None
autocmd FocusGained,BufEnter,CursorHold,CursorHoldI * if mode() !~ '\v(c|r.?|!|t)' && getcmdwintype() == '' | checktime | endif

lua <<EOF
require('leap').add_default_mappings()
require('colorizer').setup()
require('nvim-treesitter.configs').setup {
	ensure_installed = { "javascript", "typescript" },
	sync_install = false,
	auto_install = true,
	highlight = { enable = true, additional_vim_regex_highlighting = false },
}
EOF
