"""""""""""""""""""""""""
"       Plugins         "
"""""""""""""""""""""""""

if ! filereadable(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim"'))
	echo "Downloading junegunn/vim-plug to manage plugins..."
	silent !mkdir -p ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/
	silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim
	autocmd VimEnter * PlugInstall
endif

call plug#begin(split(&rtp, ',')[0] . '/plugins')
Plug 'airblade/vim-gitgutter'
Plug 'airblade/vim-rooter'
Plug 'ap/vim-css-color'
Plug 'christoomey/vim-tmux-navigator'
Plug 'dense-analysis/ale'
Plug 'hrsh7th/vim-vsnip'
Plug 'instant-markdown/vim-instant-markdown', {'for': 'markdown', 'do': 'yarn install'}
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align'
Plug 'justinmk/vim-sneak'
Plug 'ruanyl/vim-gh-line'
Plug 'mattn/emmet-vim'
Plug 'NLKNguyen/papercolor-theme'
Plug 'preservim/vimux'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-characterize'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'
Plug 'Yggdroot/indentLine'
call plug#end()

"""""""""""""""""""""""""
"       Settings        "
"""""""""""""""""""""""""

syntax on                                                  " Syntax highlighting
filetype plugin indent on                                  " Turn on filetype detections

set clipboard=unnamedplus                                  " Enables OS clipboard support
set encoding=utf-8                                         " Encoding for files
set foldmethod=syntax                                      " Set foldmethod to syntax
set history=1000                                           " Set command history to 1000
set ignorecase                                             " Ignore case of characters in search patterns
set list                                                   " Show whitespace characters
set listchars=tab:┊·,trail:·,extends:>,precedes:<,nbsp:+   " Set default whitespace characters
set nohlsearch                                             " Disable search highlighting
set noshowmode                                             " Don't show current mode in last line
set noshowcmd                                              " Don't show information relating to current mode
set noswapfile                                             " No swap files
set nowrap                                                 " Disable line wrapping
set noruler                                                " Disable ruler
set number                                                 " Show current line number and relative line numbers
set relativenumber                                         " Show relative line numbers
set scrolloff=1                                            " Minimum number of lines above and below cursor
set shortmess+=I                                           " Supress intro message
set shortmess+=c                                           " Suppress ins-completion-menu messages
set signcolumn=yes                                         " Always show signcolumn
set smartcase                                              " Override 'ignorecase' if search pattern contains uppercase characters
set statusline=%!StatusLine(1)                             " Generate statusline
set updatetime=300                                         " Set CursorHold delay time

"""""""""""""""""""""""""
"        Colors         "
"""""""""""""""""""""""""

set termguicolors
set background=dark
colorscheme PaperColor

" Remove background from all highlight groups
hi Normal              guibg=none
hi LineNr              guibg=none
hi SignColumn          guibg=none
hi ALEErrorSign        guibg=none guifg=red
hi ALEWarningSign      guibg=none guifg=orange

"""""""""""""""""""""""""
"        Globals        "
"""""""""""""""""""""""""

" FZF
let g:fzf_preview_window = ['right:50%:hidden', 'ctrl-]']
let g:fzf_layout = { 'down': '40%' }

" ALE
let g:ale_sign_error = '•'
let g:ale_sign_warning = '•'
let g:ale_completion_enabled = 1
let g:ale_completion_autoimport = 1
let g:ale_lint_on_save = 1
let g:ale_lint_on_text_changed = 0
let g:ale_lint_on_insert_leave = 0
let g:ale_linters_explicit = 1
let g:ale_echo_msg_error_str = 'ERR'
let g:ale_echo_msg_warning_str = 'WARN'
let g:ale_echo_msg_format = '[%linter%][%severity%][%code%] %s'
let g:ale_linters = { 
			\ 'go': ['gopls'],
			\ 'javascript': ['tsserver', 'eslint'],
			\ 'javascriptreact': ['tsserver', 'eslint'],
			\ 'typescript': ['tsserver', 'eslint'],
			\ 'typescriptreact': ['tsserver', 'eslint'],
			\}
let g:ale_fixers = { 
			\ 'go': ['gofmt'],
			\ 'scala': ['scalafmt'],
			\ 'svelte': ['prettier'],
			\ 'yaml': ['prettier'],
			\ 'javascript': ['prettier', 'eslint'],
			\ 'javascriptreact': ['prettier', 'eslint'],
			\ 'typescript': ['prettier', 'eslint'],
			\ 'typescriptreact': ['prettier', 'eslint'],
			\}
let g:ale_pattern_options = {
			\ '\.min\.js$': {'ale_linters': [], 'ale_fixers': []},
			\ '\.min\.css$': {'ale_linters': [], 'ale_fixers': []},
			\}

" Rooter
let g:rooter_silent_chdir = 1
let g:rooter_patterns = ['.git', 'Makefile', 'package.json', 'init.vim', '.envrc']
let g:rooter_change_directory_for_non_project_files = 'current'

" Instant markdown preview
let g:instant_markdown_autostart = 0

" VSnip
let g:vsnip_filetypes = {}
let g:vsnip_filetypes.javascriptreact = ['javascript']
let g:vsnip_filetypes.typescriptreact = ['typescript']
let g:vsnip_snippet_dir = system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/snippets"')

" IndentLine
let g:indentLine_fileType = ['yaml', 'yml']

" GH line
let g:gh_open_command = 'fn() { echo "$@" | pbcopy; }; fn '

" Emmet
let g:user_emmet_install_global = 0
autocmd FileType html,css EmmetInstall

"""""""""""""""""""""""
"      Mappings       "
"""""""""""""""""""""""

" Make <SPACE> the leader
let mapleader = " "
let maplocalleader = " "

" Break undo before deleting a word
inoremap <C-W> <C-G>u<C-W>
" Break undo before deleting a line
inoremap <C-U> <C-G>u<C-U>

nmap          R              :%s//g<Left><Left>
nmap <silent> ]g             <plug>(ale_next)
nmap <silent> [g             <plug>(ale_previous)
nmap <silent> gd             <plug>(ale_go_to_definition)
nmap <silent> K              <plug>(ShowDocumentation)
nmap <silent> yoa            :ALEToggleBuffer<CR>
nmap <silent> yoz            <plug>(ZoomToggle)
nmap <silent> <leader>b      :Buffers<CR>
nmap <silent> <leader>g      :Git<CR>
nmap <silent> <leader>j      :sp<CR>
nmap <silent> <leader>l      :vsp<CR>
nmap <silent> <leader>o      :Files<CR>
nmap <silent> <leader>p      :Commands<CR>
nmap <silent> <leader>q      :q<CR>
nmap <silent> <leader>t      :Helptags<CR>
nmap <silent> <leader>w      :w<CR>
nmap <silent> <leader>/      :Rg<CR>
nmap <silent> <leader>.      :GFiles<CR>
nmap <silent> <leader><tab>  <plug>(fzf-maps-n)
nmap <silent> <leader>rn     <plug>(ale_rename)
nmap <silent> <leader>hh     <plug>(ToggleStatusline)
nmap <silent> <leader>yf     :let @*=expand("%:p")<CR>
nmap <silent> <leader>ec     :e   ~/.config/nvim/init.vim<CR>
nmap <silent> <leader>so     :so  ~/.config/nvim/init.vim<CR>
nmap <silent> <leader>=h     :exe "resize +5"<CR>
nmap <silent> <leader>-h     :exe "resize -5"<CR>
nmap <silent> <leader>=v     :exe "vertical resize +5"<CR>
nmap <silent> <leader>-v     :exe "vertical resize -5"<CR>

omap <silent> <leader><tab>  <plug>(fzf-maps-o)

xmap <silent> <leader>/      <plug>(SearchRange)
xmap <silent> <leader>ea     <plug>(EasyAlign)
xmap <silent> <leader><tab>  <plug>(fzf-maps-x)

imap <silent> <c-x><c-k>     <plug>(fzf-complete-word)
imap <silent> <c-x><c-l>     <plug>(fzf-complete-line)
imap <silent> <c-x><c-x>     <plug>(fzf-complete-snippet)

imap <silent> <expr> <C-j>   vsnip#expandable() ? '<plug>(vsnip-expand)'         : '<C-j>'
imap <silent> <expr> <C-l>   vsnip#available(1) ? '<plug>(vsnip-expand-or-jump)' : '<C-l>'
imap <silent> <expr> <Tab>   vsnip#jumpable(1)  ? '<plug>(vsnip-jump-next)'      : (pumvisible() ? '<C-n>' : '<plug>(SmartTabComplete)')
imap <silent> <expr> <S-Tab> vsnip#jumpable(-1) ? '<plug>(vsnip-jump-prev)'      : (pumvisible() ? '<C-p>' : '<C-h>')
smap <silent> <expr> <C-j>   vsnip#expandable() ? '<plug>(vsnip-expand)'         : '<C-j>'
smap <silent> <expr> <C-l>   vsnip#available(1) ? '<plug>(vsnip-expand-or-jump)' : '<C-l>'
smap <silent> <expr> <Tab>   vsnip#jumpable(1)  ? '<plug>(vsnip-jump-next)'      : '<Tab>'
smap <silent> <expr> <S-Tab> vsnip#jumpable(-1) ? '<plug>(vsnip-jump-prev)'      : '<S-Tab>'

"""""""""""""""""""""""""
"       Commands        "
"""""""""""""""""""""""""

command! Gqf GitGutterQuickFix | copen

autocmd FileType           gitcommit,gitrebase,gitconfig set bufhidden=delete
autocmd BufNewFile,BufRead init.vim                      let g:gitgutter_git_args='--git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

function! s:empty_echo(t)
	if mode() ==# 'n'
		echon ''
	endif
endfunction

augroup cmd_msg_cls
	autocmd!
	autocmd CmdlineLeave * call timer_start(1500, funcref('s:empty_echo'))
augroup END

augroup statusline_commands
	autocmd!
	autocmd VimEnter * call ToggleStatusline()
	autocmd WinEnter * setlocal statusline=%!StatusLine(1)
	autocmd WinLeave * setlocal statusline=%!StatusLine(0)
augroup END

"""""""""""""""""""""""""
"        Netrw          "
"""""""""""""""""""""""""

" Keep the current directory and the browsing directory synced.
" This helps you avoid the move files error.
let g:netrw_keepdir = 0
" Enable recursive copy of directories
let g:netrw_localcopydircmd = 'cp -r'

" Highlight marked files in the same way search matches are.
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
	" copy to target
	nmap <buffer> fc mc
	" set target and copy
	nmap <buffer> fC mtmc
	" set target
	nmap <buffer> ft mt
	" move to target
	nmap <buffer> fx mm
	" set target and move
	nmap <buffer> fX mtmm
	" run command on file
	nmap <buffer> f; mx
	" remove recursively
	nmap <buffer> FD :call NetrwRemoveRecursive()<CR>
endfunction

augroup netrw_mapping
  autocmd!
  autocmd filetype netrw call NetrwMapping()
augroup END
