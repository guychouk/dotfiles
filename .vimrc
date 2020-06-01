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
Plug 'morhetz/gruvbox'
Plug 'godlygeek/tabular'
Plug 'sheerun/vim-polyglot'
Plug 'airblade/vim-gitgutter'
Plug 'ludovicchabant/vim-gutentags'
Plug 'airblade/vim-rooter'
Plug 'junegunn/goyo.vim'
Plug 'mattn/emmet-vim'
Plug 'pangloss/vim-javascript'
Plug 'dense-analysis/ale'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'psliwka/vim-smoothie'
Plug 'preservim/nerdtree'
Plug 'tpope/vim-surround'
call plug#end()

"""""""""""""""""""""""""
"        General        "
"""""""""""""""""""""""""

syntax on                  " Syntax highlighting
colorscheme gruvbox 	   " Gruvbox theme
filetype plugin indent on  " Turn on filetype detections

" Hightlights
highlight clear SignColumn
highlight link ALEErrorSign   GruvboxRed
highlight link ALEInfoSign    GruvboxWhite
highlight link ALEWarningSign GruvboxYellow

" Options
set backspace=2       " Fix <BS> key
set encoding=utf-8    " Encoding for files
set shiftwidth=4      " Assits with code formatting
set foldlevel=99      " Unfolds all folds by default
set foldlevelstart=1  " Fold method
set foldmethod=syntax " Set foldmethod for programming
set tabstop=4         " Sets tab to 4 instead of vim's crazy 8
set clipboard=unnamed " Mainly for better compatibility with Windows
set cmdheight=2       " More space for displaying messages
set laststatus=2      " Set statusbar to always appear (for Lightline)
set updatetime=300    " Recommended value for updatetime found in coc.vim's wiki
set shortmess+=c      " Avoid passing messages to ins-completion-menu
set signcolumn=yes    " Always show signcolumn

set nobackup          " No backup files
set nowritebackup     " No backup files
set smarttab          " Improves tabbing
set number            " Show line numbers
set nowrap            " Disable line wrapping
set incsearch         " Sets incremental search
set nohlsearch        " Disable search highlight
set noswapfile        " Disables swp files creation
set autoindent        " New lines will be indented as well
set termguicolors     " Emit true (24-bit) colors in the terminal
set smartcase         " No ignore case when pattern has uppercase
set hidden            " Hide abandoned buffers instead of unloading them

"""""""""""""""""""""""""
"       Variables       "
"""""""""""""""""""""""""

let javascript_fold = 1
let g:polyglot_disabled = ['javascript']

let mapleader = " "
let maplocalleader = " "

" NERDTree
let NERDTreeShowHidden=1

" ALE
let g:ale_sign_error = '•'
let g:ale_sign_warning = '•'
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_insert_leave = 0
let g:ale_linters_explicit = 1
let g:ale_pattern_options_enabled = 1
let g:ale_linters = {
\   'javascript': ['eslint'],
\}
let g:ale_fixers = {
\   'javascript': ['eslint'],
\}
let g:ale_pattern_options = {
\ '\.min\.js$': {'ale_linters': [], 'ale_fixers': []},
\ '\.min\.css$': {'ale_linters': [], 'ale_fixers': []},
\}

" GitGutter
let g:gitgutter_map_keys = 0
let g:gitgutter_set_sign_backgrounds = 1

" Rooter
let g:rooter_change_directory_for_non_project_files = 'current'

" Emmet (<c-y>,)
let g:user_emmet_install_global = 0
autocmd FileType html,css EmmetInstall

au BufNewFile,BufRead *.js set filetype=javascript

"""""""""""""""""""""""""
"      Status Line      "
"""""""""""""""""""""""""

set statusline=
set statusline+=%<%F\ %m\ %r\ %h                                      " File path, modified, readonly, helpfile, preview
set statusline+=%=                                                    " left/right separator
set statusline+=\ %y                                                  " filetype
set statusline+=\ %L                                                  " cursor line/total lines
set statusline+=\ %P                                                  " percentage of file
set statusline+=\ %{coc#status()}%{get(b:,'coc_current_function','')} " Show COC status
" set statusline+=%{b:gitbranch}                                      " show git branch

function! StatuslineGitBranch()
  let b:gitbranch=""
  if &modifiable
    try
      let l:dir=expand('%:p:h')
      let l:gitrevparse = system("git -C ".l:dir." rev-parse --abbrev-ref HEAD")
      if !v:shell_error
        let b:gitbranch="(".substitute(l:gitrevparse, '\n', '', 'g').") "
      endif
    catch
    endtry
  endif
endfunction

augroup GetGitBranch
  autocmd!
  autocmd VimEnter,WinEnter,BufEnter * call StatuslineGitBranch()
augroup END

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
nnoremap <Leader>. :Files<CR>
nnoremap <Leader>b :Buffers<CR>

nnoremap <Leader>ec :e ~/.vimrc<CR>
nnoremap <Leader>zc :e ~/.zshrc<CR>

nnoremap <Leader>pc :PlugClean<CR>
nnoremap <Leader>pi :PlugInstall<CR>
nnoremap <Leader>so :so ~/.vimrc<CR>

nnoremap <silent> <expr> <F6> g:NERDTree.IsOpen() ? "\:NERDTreeClose<CR>" : bufexists(expand('%')) ? "\:NERDTreeFind<CR>" : "\:NERDTree<CR>"

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

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

" Show all diagnostics.
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
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
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')
