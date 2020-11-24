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
Plug '~/.vim/custom/swift'
Plug 'airblade/vim-rooter'
Plug 'airblade/vim-gitgutter'
Plug 'dense-analysis/ale'
Plug 'godlygeek/tabular'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/goyo.vim'
Plug 'mattn/emmet-vim'
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

syntax on                  " Syntax highlighting
filetype plugin indent on  " Turn on filetype detections

set pyx=3                  " Set Python version to 3
set shortmess+=I           " Supress startup message
set shortmess+=c           " Avoid passing messages to ins-completion-menu
set updatetime=250         " Set CursorHold delay time
set encoding=utf-8         " Encoding for files
set signcolumn=yes         " Always show signcolumn
set foldmethod=manual      " Set foldmethod to be manual
set clipboard=unnamedplus  " Enables OS clipboard support

set nowrap                 " Disable line wrapping
set noswapfile             " No swap files
set nobackup               " No backup files
set nowritebackup          " No backup files
set nohlsearch             " Disable search highlight
set noshowmode             " Don't show current mode in cmdline
set number                 " Show line numbers
set incsearch              " Sets incremental search
set autoindent             " New lines will be indented as well
set smartcase              " No ignore case when pattern has uppercase
set hidden                 " Hide abandoned buffers instead of unloading them

runtime snippets.vim       " Load snippets

"""""""""""""""""""""""""
"        Colors         "
"""""""""""""""""""""""""

set termguicolors          " Emit true (24-bit) colors in the terminal
colorscheme afterglow

hi Normal guibg=none
hi SignColumn guibg=none
hi ALEErrorSign guibg=none
hi ALEWarningSign guibg=none
hi LineNr guibg=none guifg=#404040

hi User1 guifg=orange guibg=#1f2630
hi User2 guifg=grey30 guibg=#1f2630
hi User3 guifg=green guibg=#1f2630

hi StatusLine guifg=#1f2630 guibg=grey90
hi StatusLineNC guifg=#1f2630 guibg=grey30

let g:vim_jsx_pretty_colorful_config = 1

"""""""""""""""""""""""""
"        Quwiki         "
"""""""""""""""""""""""""

function! QuwikiSkeleton()
	0r ~/.config/nvim/skeleton.help
	execute '%s/:F:/' . expand('%:t')
endfunction

autocmd BufNewFile */quwiki/*.txt :call QuwikiSkeleton()

"""""""""""""""""""""""""
"       Variables       "
"""""""""""""""""""""""""

" <SPC> as leader
let mapleader = " "
let maplocalleader = " "

" NERDTree
let NERDTreeShowHidden = 1
let g:NERDTreeBookmarksFile = $XDG_CONFIG_HOME . "/nvim/.nerdtree-bookmarks"

" ALE
let g:ale_lint_delay = 250
let g:ale_sign_error = '• '
let g:ale_sign_warning = '• '
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%] [%code%]'
let g:ale_lint_on_text_changed = 'normal'
let g:ale_linters_explicit = 1
let g:ale_linters = { 
\ 'cpp': ['ccls'],
\ 'javascript': ['eslint'],
\ 'typescript': ['eslint', 'tsserver'],
\ 'typescript.tsx': ['eslint', 'tsserver'],
\}
let g:ale_fixers = { 
\ 'go': ['gofmt'],
\ 'javascript': ['eslint'],
\ 'typescript': ['eslint'],
\ 'typescript.tsx': ['eslint'],
\}
let g:ale_pattern_options = {
\ '\.min\.js$': {'ale_linters': [], 'ale_fixers': []},
\ '\.min\.css$': {'ale_linters': [], 'ale_fixers': []},
\}

" Slime
let g:slime_target = "tmux"
let g:slime_paste_file = $XDG_CACHE_HOME . "/.slime_paste"
let g:slime_default_config = {"socket_name": get(split($TMUX, ","), 0), "target_pane": ":.2"}

" Rooter
let g:rooter_silent_chdir = 1
let g:rooter_patterns = ['.git', 'Makefile', 'package.json']
let g:rooter_change_directory_for_non_project_files = 'current'

" Emmet (<c-y>,)
let g:user_emmet_install_global = 0
let g:user_emmet_expandabbr_key='<Tab>'

" vim-gh-line
let g:gh_line_map_default = 0
let g:gh_line_blame_map_default = 1

"""""""""""""""""""""""""
"       Functions       "
"""""""""""""""""""""""""

function! s:nerdtree_toggle()
    let is_open = g:NERDTree.IsOpen()
    execute is_open ? 'NERDTreeClose' : bufexists(expand('%')) ? 'NERDTreeFind' : 'NERDTree'
endfunction

function! AgRange(type, ...)
    let sel_save = &selection
    let &selection = "inclusive"
    let reg_save = @@

    if a:0  " Invoked from Visual mode, use gv command.
        silent exe "normal! gvy"
    elseif a:type == 'line'
        silent exe "normal! '[V']y"
    else
        silent exe "normal! `[v`]y"
    endif

    execute "Ag " . @@

    let &selection = sel_save
    let @@ = reg_save
endfunction

" When using `dd` in the quickfix list,
" remove selected item from the quickfix list.
function! RemoveQFItem()
  let curqfidx = line('.') - 1
  let qfall = getqflist()
  call remove(qfall, curqfidx)
  call setqflist(qfall, 'r')
  execute curqfidx + 1 . "cfirst"
  :copen
endfunction

"""""""""""""""""""""""""
"      Remappings       "
"""""""""""""""""""""""""

imap jk <Esc>

nmap <silent> ]g <Plug>(ale_next)
nmap <silent> [g <Plug>(ale_previous)
nmap <silent> <F6> :call <SID>nerdtree_toggle()<CR>

nnoremap <silent> <Leader>w :w<CR>
nnoremap <silent> <Leader>q :q<CR>
nnoremap <silent> <Leader>g :G<CR>
nnoremap <silent> <Leader>s :Ag<CR>
nnoremap <silent> <Leader>J :sp<CR>
nnoremap <silent> <Leader>L :vsp<CR>
nnoremap <silent> <Leader>. :GFiles<CR>
nnoremap <silent> <Leader>b :Buffers<CR>
nnoremap <silent> <leader>/ :set operatorfunc=AgRange<cr>g@
vnoremap <silent> <leader>/ :<c-u>call AgRange(visualmode(), 1)<cr>

nnoremap <silent> <Leader>yf :let @*=expand("%:p")<CR>
nnoremap <silent> <Leader>zc :e ~/.config/zsh/.zshrc<CR>
nnoremap <silent> <Leader>ec :e ~/.config/nvim/init.vim<CR>
nnoremap <silent> <Leader>so :so ~/.config/nvim/init.vim<CR>

nnoremap <silent> <Leader>=h :exe "resize " . (winheight(0) * 4/3)<CR>
nnoremap <silent> <Leader>=v :exe "vertical resize " . (winwidth(0) * 4/3)<CR>
nnoremap <silent> <Leader>-h :exe "resize " . (winheight(0) * 3/4)<CR>
nnoremap <silent> <Leader>-v :exe "vertical resize " . (winwidth(0) * 3/4)<CR>

"""""""""""""""""""""""""
"     Autocommands      "
"""""""""""""""""""""""""

autocmd BufWritePost * GitGutter
autocmd FileType gitcommit,gitrebase,gitconfig set bufhidden=delete

autocmd FileType qf map <silent> <buffer> dd :RemoveQFItem<cr>

autocmd BufNewFile,BufRead *.js set filetype=javascript
autocmd BufNewFile,BufRead *.jsx set filetype=javascript.jsx
autocmd BufNewFile,BufRead *.ts set filetype=typescript
autocmd BufNewFile,BufRead *.tsx set filetype=typescript.tsx

autocmd FileType html,css EmmetInstall
autocmd FileType html setlocal shiftwidth=2
autocmd FileType javascript,javascript.tsx setlocal ts=2 sts=2 sw=2
autocmd FileType typescript,typescript.tsx setlocal ts=2 sts=2 sw=2
autocmd FileType json setlocal ts=2 sts=2 sw=2 formatexpr=CocAction('formatSelected')

"""""""""""""""""""""""""
"       Commands        "
"""""""""""""""""""""""""

" Remove entry from quickfix list
command! RemoveQFItem :call RemoveQFItem()

" Remove quotes from JS object keys
command! -range=% Rq <line1>,<line2>normal 0ds"j

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call CocAction('fold', <f-args>)

" Show only changed files
command! Fzfc :call fzf#run(fzf#wrap({'source': 'git ls-files --exclude-standard --others --modified'}))

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

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at 
" current position. Coc only does snippet and additional edit on confirm.
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
nmap <silent> gr <Plug>(coc-references)
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gy <Plug>(coc-type-definition)

" Coc Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

" Find symbol of current document.
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
