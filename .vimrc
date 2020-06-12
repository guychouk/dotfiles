"""""""""""""""""""""""""
"       vim-plug        "
"""""""""""""""""""""""""

if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
                \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

if has('win32') || has('win64')
    let &shell='cmd.exe'
endif

call plug#begin(split(&rtp, ',')[0] . '/plugins')
Plug 'danilo-augusto/vim-afterglow'
Plug 'psliwka/vim-smoothie'
Plug 'godlygeek/tabular'
Plug 'airblade/vim-rooter'
Plug 'sheerun/vim-polyglot'
Plug 'airblade/vim-gitgutter'
Plug 'junegunn/goyo.vim'
Plug 'mattn/emmet-vim'
Plug 'dense-analysis/ale'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'preservim/nerdtree'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'mattn/vim-gist'
Plug 'mattn/webapi-vim'
Plug 'editorconfig/editorconfig-vim'
call plug#end()

"""""""""""""""""""""""""
"        General        "
"""""""""""""""""""""""""

syntax on                  " Syntax highlighting
filetype plugin indent on  " Turn on filetype detections
colorscheme afterglow      " Afterglow color scheme

set clipboard=unnamed " ?
set encoding=utf-8    " Encoding for files
set foldmethod=manual " Set foldmethod for programming
set signcolumn=yes    " Always show signcolumn
set shortmess+=c      " Avoid passing messages to ins-completion-menu

set nobackup          " No backup files
set nowritebackup     " No backup files
set smarttab          " Improves tabbing
set number            " Show line numbers
set nowrap            " Disable line wrapping
set incsearch         " Sets incremental search
set nohlsearch        " Disable search highlight
set noswapfile        " Disables swp files creation
set expandtab         " Insert spaces when tab is pressed
set autoindent        " New lines will be indented as well
set termguicolors     " Emit true (24-bit) colors in the terminal
set smartcase         " No ignore case when pattern has uppercase
set hidden            " Hide abandoned buffers instead of unloading them

"""""""""""""""""""""""""
"       Variables       "
"""""""""""""""""""""""""

let mapleader = " "
let maplocalleader = " "

" NERDTree
let NERDTreeShowHidden = 1

" ALE
let g:ale_sign_error = '•'
let g:ale_sign_warning = '•'
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
let g:ale_lint_on_text_changed = 'always'
let g:ale_lint_delay = 1000
let g:ale_linters_explicit = 1
let g:ale_linters = { 'javascript': ['eslint'] }
let g:ale_fixers = { 'javascript': ['eslint'] }
let g:ale_pattern_options = {
\ '\.min\.js$': {'ale_linters': [], 'ale_fixers': []},
\ '\.min\.css$': {'ale_linters': [], 'ale_fixers': []},
\}

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

" Set .js files as Javascript filetype
au BufNewFile,BufRead *.js set filetype=javascript

" JS settings
autocmd FileType javascript setlocal ts=2 sts=2 sw=2

"""""""""""""""""""""""""
"      Status Line      "
"""""""""""""""""""""""""
hi statusline guibg=black ctermfg=8 guifg=plum ctermbg=15

set statusline=
set statusline+=%<%F\ %m\ %r\ %h                                      " File path, modified, readonly, helpfile, preview
set statusline+=\ %{FugitiveStatusline()}                             " Show git branch
set statusline+=%=                                                    " left/right separator
set statusline+=\ %y                                                  " filetype
set statusline+=\ %L                                                  " cursor line/total lines
set statusline+=\ %P                                                  " percentage of file
set statusline+=\ %{coc#status()}%{get(b:,'coc_current_function','')} " Show COC status

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

nnoremap <Leader>ec :e ~/.vimrc<CR>
nnoremap <Leader>zc :e ~/.zshrc<CR>

nnoremap <Leader>pc :PlugClean<CR>
nnoremap <Leader>pi :PlugInstall<CR>
nnoremap <Leader>so :so ~/.vimrc<CR>

nnoremap <silent> <expr> <F6> g:NERDTree.IsOpen() ? "\:NERDTreeClose<CR>" : bufexists(expand('%')) ? "\:NERDTreeFind<CR>" : "\:NERDTree<CR>"

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

function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
    else
        call CocAction('doHover')
    endif
endfunction

" Manage extensions.
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

"""""""""""""""""""""""""
"        Coc.vim        "
"""""""""""""""""""""""""

" Use tab for trigger completion with characters ahead and navigate.
inoremap <silent><expr> <TAB>
            \ pumvisible() ? "\<C-n>" :
            \ <SID>check_back_space() ? "\<TAB>" :
            \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
" <cr> could be remapped by other vim plugin, try `:verbose imap <CR>`.
if exists('*complete_info')
    inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
    inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

augroup mygroup
    autocmd!
    " Setup formatexpr specified filetype(s).
    autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
    " Update signature help on jump placeholder.
    autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Introduce function text object
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR :call CocAction('runCommand', 'editor.action.organizeImport')

" Show only changed files
command! Fzfc :call fzf#run(fzf#wrap({'source': 'git ls-files --exclude-standard --others --modified'}))
