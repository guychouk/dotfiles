"""""""""""""""""""""""""
"       vim-plug        "
"""""""""""""""""""""""""

if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
                \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin(split(&rtp, ',')[0] . '/plugins')
Plug 'airblade/vim-rooter'
Plug 'airblade/vim-gitgutter'
Plug 'dense-analysis/ale'
Plug 'godlygeek/tabular'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/goyo.vim'
Plug 'mattn/emmet-vim'
Plug 'mattn/vim-gist'
Plug 'mattn/webapi-vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'preservim/nerdtree'
Plug 'ruanyl/vim-gh-line'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'jpalardy/vim-slime'
Plug 'yuezk/vim-js'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'danilo-augusto/vim-afterglow'
Plug '~/.vim/custom/swift'
Plug 'christoomey/vim-tmux-navigator'
call plug#end()

"""""""""""""""""""""""""
"        General        "
"""""""""""""""""""""""""

" Configure theme & colors
colorscheme afterglow     
hi Normal guibg=none
hi LineNr guibg=none
hi SignColumn guibg=none
hi ALEErrorSign guibg=none
hi ALEWarningSign guibg=none

syntax on                 " Syntax highlighting
filetype plugin indent on " Turn on filetype detections

set encoding=utf-8        " Encoding for files
set foldmethod=manual     " Set foldmethod for programming
set signcolumn=yes        " Always show signcolumn
set shortmess+=c          " Avoid passing messages to ins-completion-menu
set shortmess+=I          " Supress startup message
set clipboard=unnamedplus " Enables OS clipboard support (for WSL as well)
set pyx=3                 " Set Python version to 3

set tabstop=4             " Width of a hard tabstop measured in spaces
set shiftwidth=4          " When indenting with '>', use 4 spaces width
set expandtab             " Make the tab key insert spaces instead of tab characters

set nobackup              " No backup files
set nowritebackup         " No backup files
set smarttab              " Improves tabbing
set number                " Show line numbers
set nowrap                " Disable line wrapping
set incsearch             " Sets incremental search
set nohlsearch            " Disable search highlight
set noswapfile            " Disables swp files creation
set expandtab             " Insert spaces when tab is pressed
set autoindent            " New lines will be indented as well
set termguicolors         " Emit true (24-bit) colors in the terminal
set smartcase             " No ignore case when pattern has uppercase
set hidden                " Hide abandoned buffers instead of unloading them
set relativenumber        " Set line numbers relative to cursor

runtime snippets.vim      " Load snippets

"""""""""""""""""""""""""
"       Variables       "
"""""""""""""""""""""""""
let mapleader = " "
let maplocalleader = " "

" NERDTree
let NERDTreeShowHidden = 1

" ALE
let g:ale_lint_delay = 250
let g:ale_sign_error = '•'
let g:ale_sign_warning = '•'
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%] [%code%]'
let g:ale_lint_on_text_changed = 'always'
let g:ale_linters_explicit = 1
let g:ale_linters = { 
\ 'javascript': ['eslint'],
\ 'typescript': ['eslint', 'tsserver'],
\ 'typescript.tsx': ['eslint', 'tsserver'],
\}
let g:ale_pattern_options = {
\ '\.min\.js$': {'ale_linters': [], 'ale_fixers': []},
\ '\.min\.css$': {'ale_linters': [], 'ale_fixers': []},
\}

" FileType settings
au BufNewFile,BufRead *.js set filetype=javascript
au BufNewFile,BufRead *.jsx set filetype=javascript.jsx
au BufNewFile,BufRead *.ts set filetype=typescript
au BufNewFile,BufRead *.tsx set filetype=typescript.tsx
autocmd FileType json setlocal ts=2 sts=2 sw=2
autocmd FileType javascript,javascript.tsx setlocal ts=2 sts=2 sw=2
autocmd FileType typescript,typescript.tsx setlocal ts=2 sts=2 sw=2

" JSX Syntax Highlighting
let g:vim_jsx_pretty_colorful_config = 1

" Vim-Slime
let g:slime_target = "tmux"
let g:slime_default_config = {"socket_name": get(split($TMUX, ","), 0), "target_pane": ":.2"}

" GitGutter
set updatetime=250
autocmd BufWritePost * GitGutter

" Delete buffer after git commit, rebase or config
autocmd FileType gitcommit,gitrebase,gitconfig set bufhidden=delete

" Rooter
let g:rooter_change_directory_for_non_project_files = 'current'

" Emmet (<c-y>,)
let g:user_emmet_install_global = 0
autocmd FileType html,css EmmetInstall
autocmd FileType html set shiftwidth=2

"""""""""""""""""""""""""
"      Status Line      "
"""""""""""""""""""""""""
hi statusline guifg=grey20 guibg=white ctermbg=15 ctermfg=8

set statusline=
set statusline+=\ %<%F\ %m\ %r\ %h                                    " File path, modified, readonly, helpfile, preview
set statusline+=%=                                                    " Add left/right separator
set statusline+=\ %{FugitiveStatusline()}                             " Current git branch
set statusline+=\ %y                                                  " Show FileType
set statusline+=\ %{coc#status()}%{get(b:,'coc_current_function','')} " COC status (if available)

"""""""""""""""""""""""""
"       Functions       "
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

function! s:nerdtree_toggle()
        let is_open = g:NERDTree.IsOpen()
        execute is_open ? 'NERDTreeClose' : bufexists(expand('%')) ? 'NERDTreeFind' : 'NERDTree'
endfunction

"""""""""""""""""""""""""
"     Autocommands      "
"""""""""""""""""""""""""

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

augroup mygroup
    autocmd!
    " Setup formatexpr specified filetype(s).
    autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
    " Update signature help on jump placeholder.
    autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

"""""""""""""""""""""""""
"       Commands        "
"""""""""""""""""""""""""

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR :call CocAction('runCommand', 'editor.action.organizeImport')

" Show only changed files
command! Fzfc :call fzf#run(fzf#wrap({'source': 'git ls-files --exclude-standard --others --modified'}))

"""""""""""""""""""""""""
"      Remappings       "
"""""""""""""""""""""""""

imap jk <Esc>

nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
nnoremap <Leader>w :w<CR>
nnoremap <Leader>q :q<CR>

nnoremap <Leader>J :sp<CR>
nnoremap <Leader>L :vsp<CR>

nnoremap <Leader>/ :Ag<CR>
nnoremap <Leader>. :GFiles<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>gg :G<CR>

nnoremap <Leader>ec :e ~/.vimrc<CR>
nnoremap <Leader>zc :e ~/.zshrc<CR>

nnoremap <Leader>pc :PlugClean<CR>
nnoremap <Leader>pi :PlugInstall<CR>
nnoremap <Leader>so :so ~/.vimrc<CR>

nnoremap <silent> <F6> :call <SID>nerdtree_toggle()<CR>

" Navigate ALE diagnostics
nmap <silent> [g <Plug>(ale_previous)
nmap <silent> ]g <Plug>(ale_next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

" Manage extensions.
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>

"""""""""""""""""""""""""
"        Coc.vim        "
"""""""""""""""""""""""""

" Use tab for trigger completion with characters ahead and navigate.
inoremap <silent><expr> <TAB>
            \ pumvisible() ? "\<C-n>" :
            \ <SID>check_back_space() ? "\<TAB>" :
            \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
" <cr> could be remapped by other vim plugin, try `:verbose imap <CR>`.
if exists('*complete_info')
    inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
    inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Add function text object
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)
