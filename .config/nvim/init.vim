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
Plug 'ayu-theme/ayu-vim'
Plug 'christoomey/vim-tmux-navigator'
Plug 'dense-analysis/ale'
Plug 'godlygeek/tabular'
Plug 'hrsh7th/vim-vsnip'
Plug 'instant-markdown/vim-instant-markdown', {'for': 'markdown', 'do': 'yarn install'}
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'justinmk/vim-sneak'
Plug 'ruanyl/vim-gh-line'
Plug 'preservim/vimux'
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
set nohlsearch                                             " Disable search highlight
set noshowmode                                             " Don't show current mode in status line
set noswapfile                                             " No swap files
set nowrap                                                 " Disable line wrapping
set number                                                 " Show current line number and relative line numbers
set relativenumber                                         " Show relative line numbers
set scrolloff=1                                            " Minimum number of lines above and below cursor
set shortmess+=I                                           " Supress intro message
set shortmess+=c                                           " Suppress ins-completion-menu messages
set signcolumn=yes                                         " Always show signcolumn
set smartcase                                              " Override 'ignorecase' if the search pattern contains uppercase characters
set statusline=%!StatusLine(1)                             " Generate statusline
set updatetime=300                                         " Set CursorHold delay time

"""""""""""""""""""""""""
"        Colors         "
"""""""""""""""""""""""""

" Allow color schemes to do bright colors without forcing bold.
if &t_Co == 8 && $TERM !~# '^Eterm'
	set t_Co=16
endif

set termguicolors
colorscheme ayu

" Remove background
hi Normal     guibg=none
hi LineNr     guibg=none
hi SignColumn guibg=none

let g:guibg_color = '#212121'

" User1:        Default color
" User2:        Inactive pane
" User3:        Insert Mode
" User4:        Command Mode
" User5:        Visual Mode
" Statusline:   Active Statusline
" StatuslineNC: Inactive Statusline

exe 'hi User1 guifg=orange guibg='        . g:guibg_color
exe 'hi User2 guifg=grey30 guibg='        . g:guibg_color
exe 'hi User3 guifg=violet guibg='        . g:guibg_color
exe 'hi User4 guifg=lime   guibg='        . g:guibg_color
exe 'hi User5 guifg=yellow guibg='        . g:guibg_color
exe 'hi StatusLine   guifg=white  guibg=' . g:guibg_color
exe 'hi StatusLineNC guifg=grey30 guibg=' . g:guibg_color

"""""""""""""""""""""""""
"        Globals        "
"""""""""""""""""""""""""

" FZF
let g:fzf_preview_window = ['right:50%:hidden', 'ctrl-]']
let g:fzf_layout = { 'down': '60%' }

" ALE
let g:ale_sign_error = '• '
let g:ale_sign_warning = '• '
let g:ale_completion_enabled = 1
let g:ale_completion_autoimport = 1
let g:ale_lint_on_save = 1
let g:ale_lint_on_text_changed = 0
let g:ale_lint_on_insert_leave = 0
let g:ale_linters_explicit = 1
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%][%severity%][%code%] %s'
let g:ale_cpp_cc_executable = 'g++'
let g:ale_cpp_cc_options = '-std=c++11 -Iinclude'
let g:ale_linters = { 
			\ 'cs': ['csc'],
			\ 'go': ['gopls'],
			\ 'cpp': ['g++'],
			\ 'javascript': ['tsserver', 'eslint'],
			\ 'javascriptreact': ['tsserver', 'eslint'],
			\ 'typescript': ['tsserver', 'eslint'],
			\ 'typescriptreact': ['tsserver', 'eslint'],
			\}
let g:ale_fixers = { 
			\ 'go': ['gofmt'],
			\ 'cs': ['dotnet-format'],
			\ 'scala': ['scalafmt'],
			\ 'svelte': ['prettier'],
			\ 'yaml': ['prettier'],
			\ 'cpp': ['clang-format'],
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
let g:rooter_patterns = ['.git', 'Makefile', 'package.json', 'init.vim']
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

nmap          R                    :%s//g<Left><Left>
nmap <silent> <f9>                 :Helptags<CR>
nmap <silent> ]g                   <plug>(ale_next)
nmap <silent> [g                   <plug>(ale_previous)
nmap <silent> gd                   <plug>(ale_go_to_definition)
nmap <silent> [qd                  <plug>(QuickfixRemoveEntry)
nmap <silent> K                    <plug>(ShowDocumentation)
nmap <silent> yoa                  :ALEToggleBuffer<CR>
nmap <silent> <leader>g            :Git<CR>
nmap <silent> <leader>w            :w<CR>
nmap <silent> <leader>q            :q<CR>
nmap <silent> <leader>j            :sp<CR>
nmap <silent> <leader>l            :vsp<CR>
nmap <silent> <leader>/            :Rg<CR>
nmap <silent> <leader>o            :Files<CR>
nmap <silent> <leader>.            :GFiles<CR>
nmap <silent> <leader>b            :Buffers<CR>
nmap <silent> <leader>p            :Commands<CR>
nmap <silent> <leader>rn           <plug>(ale_rename)
nmap <silent> <leader>hh           <plug>(ToggleStatusline)
nmap <silent> <leader>yf           :let @*=expand("%:p")<CR>
nmap <silent> <leader>ec           :e   ~/.config/nvim/init.vim<CR>
nmap <silent> <leader>so           :so  ~/.config/nvim/init.vim<CR>
nmap <silent> <leader>=h           :exe "resize +5"<CR>
nmap <silent> <leader>-h           :exe "resize -5"<CR>
nmap <silent> <leader>=v           :exe "vertical resize +5"<CR>
nmap <silent> <leader>-v           :exe "vertical resize -5"<CR>
nmap <silent> <leader>z            <plug>(ZoomToggle)
nmap <silent> <leader><tab>        <plug>(fzf-maps-n)

omap <silent> <leader><tab>        <plug>(fzf-maps-o)

xmap <silent> ga                   <plug>(EasyAlign)
xmap <silent> <leader>/            <plug>(SearchRange)
xmap <silent> <leader><tab>        <plug>(fzf-maps-x)

imap <silent> <c-x><c-k>           <plug>(fzf-complete-word)
imap <silent> <c-x><c-l>           <plug>(fzf-complete-line)
imap <silent> <c-x><c-i>           <plug>(fzf-complete-snippet)

imap <silent> <expr> <C-j>         vsnip#expandable() ? '<plug>(vsnip-expand)'         : '<C-j>'
imap <silent> <expr> <C-l>         vsnip#available(1) ? '<plug>(vsnip-expand-or-jump)' : '<C-l>'

imap <silent> <expr> <Tab>         vsnip#jumpable(1)  ? '<plug>(vsnip-jump-next)'      : (pumvisible() ? '<C-n>' : '<plug>(SmartTabComplete)')
imap <silent> <expr> <S-Tab>       vsnip#jumpable(-1) ? '<plug>(vsnip-jump-prev)'      : (pumvisible() ? '<C-p>' : '<C-h>')

smap <silent> <expr> <C-j>         vsnip#expandable() ? '<plug>(vsnip-expand)'         : '<C-j>'
smap <silent> <expr> <C-l>         vsnip#available(1) ? '<plug>(vsnip-expand-or-jump)' : '<C-l>'
smap <silent> <expr> <Tab>         vsnip#jumpable(1)  ? '<plug>(vsnip-jump-next)'      : '<Tab>'
smap <silent> <expr> <S-Tab>       vsnip#jumpable(-1) ? '<plug>(vsnip-jump-prev)'      : '<S-Tab>'

"""""""""""""""""""""""""
"       Commands        "
"""""""""""""""""""""""""

command! Gqf GitGutterQuickFix | copen

function! s:empty_echo(t)
	if mode() ==# 'n'
		echon ''
	endif
endfunction

autocmd FileType           gitcommit,gitrebase,gitconfig set bufhidden=delete
autocmd BufNewFile,BufRead init.vim                      let g:gitgutter_git_args='--git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

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
" Netrw split size
let g:netrw_winsize = 24
" Enable recursive copy of directories
let g:netrw_localcopydircmd = 'cp -r'

" Highlight marked files in the same way search matches are.
hi! link netrwMarkFile Search 

function! NetrwMapping()
	" VinegarUp using h
	nmap <buffer> h -
	" l to open
	nmap <buffer> l <CR>
	" abort Netrw
	nmap <buffer> <C-[> <C-^>
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
	nmap <buffer> FF :call NetrwRemoveRecursive()<CR>
endfunction

augroup netrw_mapping
  autocmd!
  autocmd filetype netrw call NetrwMapping()
augroup END
