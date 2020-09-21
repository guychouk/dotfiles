"""""""""""""""""""""""""
"       vim-plug        "
"""""""""""""""""""""""""

if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
                \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin(split(&rtp, ',')[0] . '/plugins')
Plug '~/.vim/custom/swift'
Plug 'airblade/vim-rooter'
Plug 'airblade/vim-gitgutter'
Plug 'dense-analysis/ale'
Plug 'godlygeek/tabular'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/goyo.vim'
Plug 'mattn/emmet-vim'
Plug 'mattn/vim-gist'
Plug 'mattn/webapi-vim'
Plug 'preservim/nerdtree'
Plug 'ruanyl/vim-gh-line'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'jpalardy/vim-slime'
Plug 'yuezk/vim-js'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'danilo-augusto/vim-afterglow'
Plug 'andys8/vim-elm-syntax'
Plug 'christoomey/vim-tmux-navigator'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
call plug#end()

"""""""""""""""""""""""""
"       Settings        "
"""""""""""""""""""""""""

colorscheme afterglow

syntax on                  " Syntax highlighting
filetype plugin indent on  " Turn on filetype detections

set pyx=3                  " Set Python version to 3
set updatetime=250         " Set CursorHold delay time
set tabstop=4              " Width of a hard tabstop measured in spaces
set shiftwidth=4           " When indenting with '>', use 4 spaces width
set encoding=utf-8         " Encoding for files
set signcolumn=yes         " Always show signcolumn
set shortmess+=c           " Avoid passing messages to ins-completion-menu
set shortmess+=I           " Supress startup message
set foldmethod=manual      " Set foldmethod to be manual
set clipboard=unnamedplus  " Enables OS clipboard support

set noswapfile             " No swap files
set nobackup               " No backup files
set nowritebackup          " No backup files
set smarttab               " Improves tabbing
set number                 " Show line numbers
set nowrap                 " Disable line wrapping
set incsearch              " Sets incremental search
set nohlsearch             " Disable search highlight
set expandtab              " Insert spaces when tab is pressed
set noshowmode             " Don't show current mode in cmdline
set autoindent             " New lines will be indented as well
set smartcase              " No ignore case when pattern has uppercase
set hidden                 " Hide abandoned buffers instead of unloading them
set expandtab              " Make the tab key insert spaces instead of tab characters
set termguicolors          " Emit true (24-bit) colors in the terminal

runtime snippets.vim       " Load snippets

let mapleader = " "
let maplocalleader = " "

"""""""""""""""""""""""""
"       Variables       "
"""""""""""""""""""""""""

" NERDTree
let NERDTreeShowHidden = 1

" ALE
let g:ale_lint_delay = 250
let g:ale_sign_error = '• '
let g:ale_sign_warning = '• '
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%] [%code%]'
let g:ale_lint_on_text_changed = 'always'
let g:ale_linters_explicit = 1
let g:ale_linters = { 
\ 'cpp': ['ccls'],
\ 'javascript': ['eslint'],
\ 'typescript': ['eslint', 'tsserver'],
\ 'typescript.tsx': ['eslint', 'tsserver'],
\}
let g:ale_pattern_options = {
\ '\.min\.js$': {'ale_linters': [], 'ale_fixers': []},
\ '\.min\.css$': {'ale_linters': [], 'ale_fixers': []},
\}
let g:vim_jsx_pretty_colorful_config = 1
let g:slime_target = "tmux"
let g:slime_default_config = {"socket_name": get(split($TMUX, ","), 0), "target_pane": ":.2"}

" GitGutter
autocmd BufWritePost * GitGutter

" Delete buffer after git commit, rebase or config
autocmd FileType gitcommit,gitrebase,gitconfig set bufhidden=delete

" Rooter
let g:rooter_silent_chdir = 1
let g:rooter_change_directory_for_non_project_files = 'current'
let g:rooter_patterns = ['.git', 'Makefile']

" Emmet (<c-y>,)
let g:user_emmet_install_global = 0
let g:user_emmet_expandabbr_key='<Tab>'

"""""""""""""""""""""""""
"       Highlights      "
"""""""""""""""""""""""""

hi Normal guibg=none
hi LineNr guibg=none guifg=#404040
hi SignColumn guibg=none
hi ALEErrorSign guibg=none
hi ALEWarningSign guibg=none

hi StatusLine guifg=#1f2630 guibg=grey90
hi StatusLineNC guifg=#1f2630 guibg=grey30

hi User1 guifg=orange guibg=#1f2630
hi User2 guifg=grey30 guibg=#1f2630
hi User3 guifg=green guibg=#1f2630

"""""""""""""""""""""""""
"      Statusline       "
"""""""""""""""""""""""""

let g:statusline_mode_map = {
    \ 'i'  : 'I',
    \ '!'  : '!',
    \ 't'  : 'B',
    \ 'R'  : 'R', 'Rv' : 'R',
    \ 'n'  : 'N',  'no' : 'N',
    \ 'v'  : 'V',  'V'  : 'V',  "\<C-V>" : 'V',
    \ 's'  : 'S',  'S'  : 'S',  "<\C-S>" : 'S',
    \ 'c'  : 'C', 'cv' : 'C', 'ce' : 'C',
    \ 'r'  : 'PROMPT',  'rm' : 'PROMPT',  'r?' : 'PROMPT',
    \ }

function! StatusLine(active)
    let l:icon = a:active ? '⎡•◡•⎤' : '⎡ᴗ˳ᴗ⎤'
    let l:mode_color = a:active ? '%1*' : '%2*'
    let l:filetype = empty(&filetype) ? 'N/A' : '%{&filetype}'
    let l:fugitive = a:active && FugitiveHead() != '' ? '%3*●%* %{FugitiveHead()}' : ''
    let l:coc_status = get(g:, 'coc_status', '') != '' ? '｜%{tolower(coc#status())}' : ''
    let l:statusline_segments = [
                \ l:mode_color . '⎡%{g:statusline_mode_map[mode()]}⎤' . '%*',
                \ '   ',
                \ '%{expand("%")} %m %r %h',
                \ '%=',
                \ l:fugitive,
                \ '   ',
                \ l:filetype . l:coc_status,
                \ '   ',
                \ l:mode_color . l:icon . '%*' 
                \]
    return join(l:statusline_segments)
endfunction

set statusline=%!StatusLine(1)

augroup statusline_commands
  autocmd!
  autocmd WinEnter * setlocal statusline=%!StatusLine(1)
  autocmd WinLeave * setlocal statusline=%!StatusLine(0)
augroup END

"""""""""""""""""""""""""
"       Functions       "
"""""""""""""""""""""""""

function! s:nerdtree_toggle()
    let is_open = g:NERDTree.IsOpen()
    execute is_open ? 'NERDTreeClose' : bufexists(expand('%')) ? 'NERDTreeFind' : 'NERDTree'
endfunction

"""""""""""""""""""""""""
"      Remappings       "
"""""""""""""""""""""""""

imap jk <Esc>
nmap <silent> ]g <Plug>(ale_next)
nmap <silent> [g <Plug>(ale_previous)
nnoremap <silent> <F6> :call <SID>nerdtree_toggle()<CR>

nnoremap <silent> <Leader>w :w<CR>
nnoremap <silent> <Leader>q :q<CR>
nnoremap <silent> <Leader>J :sp<CR>
nnoremap <silent> <Leader>L :vsp<CR>
nnoremap <silent> <Leader>/ :Ag<CR>
nnoremap <silent> <Leader>. :GFiles<CR>
nnoremap <silent> <Leader>b :Buffers<CR>
nnoremap <silent> <Leader>gg :G<CR>
nnoremap <silent> <Leader>ec :e ~/.vimrc<CR>
nnoremap <silent> <Leader>zc :e ~/.zshrc<CR>
nnoremap <silent> <Leader>pc :PlugClean<CR>
nnoremap <silent> <Leader>pi :PlugInstall<CR>
nnoremap <silent> <Leader>so :so ~/.vimrc<CR>
nnoremap <silent> <Leader>h= :exe "resize " . (winheight(0) * 4/3)<CR>
nnoremap <silent> <Leader>h- :exe "resize " . (winheight(0) * 3/4)<CR>
nnoremap <silent> <Leader>v= :exe "vertical resize " . (winwidth(0) * 4/3)<CR>
nnoremap <silent> <Leader>v- :exe "vertical resize " . (winwidth(0) * 3/4)<CR>

"""""""""""""""""""""""""
"     Autocommands      "
"""""""""""""""""""""""""

autocmd BufNewFile,BufRead *.js set filetype=javascript
autocmd BufNewFile,BufRead *.jsx set filetype=javascript.jsx
autocmd BufNewFile,BufRead *.ts set filetype=typescript
autocmd BufNewFile,BufRead *.tsx set filetype=typescript.tsx

autocmd FileType html setlocal shiftwidth=2
autocmd FileType html,css EmmetInstall
autocmd FileType javascript,javascript.tsx setlocal ts=2 sts=2 sw=2
autocmd FileType typescript,typescript.tsx setlocal ts=2 sts=2 sw=2
autocmd FileType json setlocal ts=2 sts=2 sw=2 formatexpr=CocAction('formatSelected')

"""""""""""""""""""""""""
"       Commands        "
"""""""""""""""""""""""""

" Remove quotes from JS object keys
command! -range=% Rq <line1>,<line2>normal 0ds"j

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call CocAction('fold', <f-args>)

" Show only changed files
command! Fzfc :call fzf#run(fzf#wrap({'source': 'git ls-files --exclude-standard --others --modified'}))

"""""""""""""""""""""""""
"        Coc.vim        "
"""""""""""""""""""""""""

function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
    else
        call CocAction('doHover')
    endif
endfunction

function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
" <cr> could be remapped by other vim plugin, try `:verbose imap <CR>`.
if exists('*complete_info')
    inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
    inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Use tab for trigger completion with characters ahead and navigate.
inoremap <silent><expr> <TAB>
            \ pumvisible() ? "\<C-n>" :
            \ <SID>check_back_space() ? "\<TAB>" :
            \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

" Add function text object
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

" Coc Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Coc GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Coc Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

" Manage extensions.
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>

" Show commands.
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>

" Find symbol of current document.
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>
