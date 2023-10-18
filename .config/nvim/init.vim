" --== Settings --==

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
let &number         = 1

" --== Plugin Settings --==

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

let g:undotree_DiffAutoOpen = 0

" --== Colors --==

colorscheme catppuccin-mocha

" --== Mappings --==

let mapleader = "\<Space>"

" Break undo before deleting a word
inoremap <C-W> <C-G>u<C-W>

" Break undo before deleting a line
inoremap <C-U> <C-G>u<C-U>

nmap          R                     :%s//g<Left><Left>
nmap          <leader>/             :Rg -g '!tags' ""<Left>
nmap          <leader>r             :History:<CR>
nmap <silent> K                     :call utils#VimHelp()<CR>
nmap <silent> yoz                   :call utils#ZoomPane()<CR>
nmap <silent> <F5>                  :UndotreeToggle<CR>
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
nmap <silent> <leader><enter>       :call utils#VimuxSlimeLine()<CR>

imap <silent>        <c-x><c-k>     <plug>(fzf-complete-word)
imap <silent>        <c-x><c-l>     <plug>(fzf-complete-line)
imap <silent>        <c-x><c-f>     <plug>(fzf-complete-path)
imap <silent> <expr> <c-x><c-x>     utils#FzfCompleteSnippet()
imap <silent> <expr> <c-x><c-o>     notes#FzfCompleteNoteLink()
imap <silent> <expr> <Tab>          vsnip#jumpable(1)  ? '<plug>(vsnip-jump-next)' : (pumvisible() ? '<C-n>' : utils#HandleTab())
imap <silent> <expr> <S-Tab>        vsnip#jumpable(-1) ? '<plug>(vsnip-jump-prev)' : (pumvisible() ? '<C-p>' : '<S-Tab>')
imap <silent> <expr> <C-j>          vsnip#expandable() ? '<plug>(vsnip-expand)'    : '<C-j>'

xmap <silent>        <leader>ea     <plug>(EasyAlign)
xmap <silent>        <leader>ys     <plug>(VSurround)
xmap <silent>        <leader><tab>  <plug>(fzf-maps-x)
xmap <silent>        <leader>/      "yy:Rg -g '!tags' "<C-R>y"<CR>

smap <silent> <expr> <C-j>          vsnip#expandable() ? '<plug>(vsnip-expand)'    : '<C-j>'
smap <silent> <expr> <Tab>          vsnip#jumpable(1)  ? '<plug>(vsnip-jump-next)' : (pumvisible() ? '<C-n>' : utils#HandleTab())
smap <silent> <expr> <S-Tab>        vsnip#jumpable(-1) ? '<plug>(vsnip-jump-prev)' : (pumvisible() ? '<C-p>' : '<S-Tab>')

omap <silent>        <leader><tab>  <plug>(fzf-maps-o)

" --== Autocommands --==

autocmd FileType gdscript
			\  let b:start='/Applications/Godot.app/Contents/MacOS/Godot'

autocmd FileType fugitive
			\  nmap <buffer> <silent> gb :call utils#FzfGitCheckoutBranch()<CR>

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
			\| compiler make

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
			\| compiler eslint
			\| let maplocalleader="\\"
			\| map <localleader>r :call VimuxRunCommand("node " . bufname("%"))<CR>
			\| map <localleader>\ :VimuxRunLastCommand<CR>
			\| EmmetInstall

autocmd FileType typescript,typescriptreact
			\  setlocal expandtab
			\| setlocal tabstop=4
			\| setlocal shiftwidth=4
			\| setlocal foldlevel=99
			\| compiler tsc
			\| let maplocalleader="\\"
			\| map <localleader>r :call VimuxRunCommand("ts-node " . bufname("%"))<CR>
			\| map <localleader>\ :VimuxRunLastCommand<CR>
			\| map <localleader>e :Dispatch -compiler=eslint<CR>
			\| EmmetInstall

autocmd FileType repl
			\  setlocal filetype=bash
			\| nmap <buffer> <silent> <enter> :call utils#VimuxSlimeLine()<CR>
			\| vmap <buffer> <silent> <enter> "vy :call utils#VimuxSlimeSelection()<CR>

autocmd FileType netrw
			\  nmap <buffer> h -
			\| nmap <buffer> l <CR>
			\| nmap <buffer> <ESC> <C-^>
			\| nmap <buffer> ff %:w<CR>:buffer #<CR>

autocmd FileType qf
			\ map <buffer> dd :call utils#RemoveQfItem()<CR>

autocmd FileType gitcommit,gitrebase,gitconfig
			\ set bufhidden=delete

autocmd BufNewFile,BufRead init.vim
			\ let g:gitgutter_git_args='--git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

autocmd! User GoyoEnter nested 
			\ call utils#GoyoEnter()

autocmd! User GoyoLeave nested
			\ call utils#GoyoLeave()

" Triger `autoread` when files change on disk
autocmd FileChangedShellPost * echohl WarningMsg | echo "File changed on disk. Buffer reloaded." | echohl None
autocmd FocusGained,BufEnter,CursorHold,CursorHoldI * if mode() !~ '\v(c|r.?|!|t)' && getcmdwintype() == '' | checktime | endif

" --== Commands --==

command!                Gqf GitGutterQuickFix | copen
command!                BuffersQf call setqflist(map(filter(range(1, bufnr('$')), 'buflisted(v:val)'), '{"bufnr":v:val}')) | copen
command! -nargs=1       ReadToQF call quickfix#ReadFileToQuickfix(<f-args>)
command! -nargs=* -bang Rg call utils#FzfExcludeFilenamesFromRg(<q-args>, <bang>0)

" --== Lua --==

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
