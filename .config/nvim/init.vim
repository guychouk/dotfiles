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
Plug 'ap/vim-css-color'
Plug 'dense-analysis/ale'
Plug 'jpalardy/vim-slime'
Plug 'preservim/nerdtree'
Plug 'Yggdroot/indentLine'
Plug 'sheerun/vim-polyglot'
Plug 'pedrohdz/vim-yaml-folds'
Plug 'kristijanhusak/vim-dadbod-ui'
Plug 'danilo-augusto/vim-afterglow'
Plug 'christoomey/vim-tmux-navigator'
Plug 'fatih/vim-go', { 'do': ':GoInstallBinaries' }
Plug 'hrsh7th/vim-vsnip'
Plug 'hrsh7th/vim-vsnip-integ'
Plug 'rafamadriz/friendly-snippets'
Plug 'airblade/vim-rooter'
Plug 'airblade/vim-gitgutter'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/vim-easy-align'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'tpope/vim-rhubarb'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-dadbod'
Plug 'instant-markdown/vim-instant-markdown', {'for': 'markdown', 'do': 'yarn install'}
call plug#end()

"""""""""""""""""""""""""
"       Settings        "
"""""""""""""""""""""""""

syntax on                 " Syntax highlighting
filetype plugin indent on " Turn on filetype detections

set autoindent            " New lines will be indented as well
set clipboard=unnamedplus " Enables OS clipboard support
set encoding=utf-8        " Encoding for files
set foldmethod=manual     " Set foldmethod to be manual
set hidden                " Hide abandoned buffers instead of unloading them
set incsearch             " Sets incremental search
set nobackup              " No backup files
set nohlsearch            " Disable search highlight
set noshowmode            " Don't show current mode in status line
set noswapfile            " No swap files
set nowrap                " Disable line wrapping
set number relativenumber " Show current line number and relative line numbers
set pyx=3                 " Set Python version to 3
set shortmess+=I          " Supress startup message
set shortmess+=c          " Don't give in-completion-menu messages
set signcolumn=yes        " Always show signcolumn
set smartcase             " No ignore case when pattern has uppercase
set updatetime=250        " Set CursorHold delay time

"""""""""""""""""""""""""
"        Colors         "
"""""""""""""""""""""""""

set termguicolors
colorscheme afterglow

" Remove background
hi Normal guibg=none
hi LineNr guibg=none
hi SignColumn guibg=none

" User1 - Default color
" User2 - Inactive pane
" User3 - Insert Mode
" User4 - Command Mode
" User5 - Visual Mode

let g:guibg_color = '#212121'

execute 'hi User1 guifg=orange guibg=' . g:guibg_color
execute 'hi User2 guifg=grey30 guibg=' . g:guibg_color
execute 'hi User3 guifg=violet guibg=' . g:guibg_color
execute 'hi User4 guifg=lime   guibg=' . g:guibg_color
execute 'hi User5 guifg=yellow guibg=' . g:guibg_color

" NOTE: For StatusLine, guifg is the background
" color and guibg is the text color ¯\(ツ)/¯
execute 'hi StatusLine   guibg=white  guifg=' . g:guibg_color
execute 'hi StatusLineNC guibg=grey30 guifg=' . g:guibg_color

"""""""""""""""""""""""""
"       Variables       "
"""""""""""""""""""""""""

" Set Python path
let g:python3_host_prog = '/usr/local/bin/python3'

" <SPC> as leader
let mapleader = " "
let maplocalleader = " "

" ALE
let g:ale_sign_error = '• '
let g:ale_sign_warning = '• '
let g:ale_lint_on_save = 1
let g:ale_linters_explicit = 1
let g:ale_completion_enabled = 1
let g:ale_lint_on_text_changed = 0
let g:ale_lint_on_insert_leave = 0
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%][%severity%][%code%] %s'
let g:ale_cpp_cc_executable = 'g++'
let g:ale_cpp_cc_options = '-std=c++11 -Iinclude'
let g:ale_linters = { 
\ 'cpp': ['g++'],
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

" Slime
let g:slime_target = "tmux"
let g:slime_paste_file = $XDG_CACHE_HOME . "/.slime_paste"
let g:slime_default_config = {"socket_name": get(split($TMUX, ","), 0), "target_pane": ":.2"}

" Rooter
let g:rooter_silent_chdir = 1
let g:rooter_patterns = ['.git', 'Makefile', 'package.json']
let g:rooter_change_directory_for_non_project_files = 'current'

" Goyo
let g:goyo_width = 85

" IndentLine
let g:indentLine_enabled = 0

" Instant markdown preview
let g:instant_markdown_autostart = 0

" Nerd tree
let NERDTreeShowHidden = 1
let NERDTreeBookmarksFile = stdpath('data') . '/NERDTreeBookmarks'
map <silent> <leader><leader> :exe g:NERDTree.IsOpen() ? 'NERDTreeClose' : bufexists(expand('%')) ? 'NERDTreeFind' : 'NERDTree'<CR>

" VSnip

" Expand
imap <silent> <expr> <C-j>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-j>'
smap <silent> <expr> <C-j>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-j>'

" Expand or jump
imap <silent> <expr> <C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'
smap <silent> <expr> <C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'

" Jump forward or backward
imap <silent> <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
smap <silent> <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
imap <silent> <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'
smap <silent> <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'

" Select or cut text to use as $TM_SELECTED_TEXT in the next snippet.
" See https://github.com/hrsh7th/vim-vsnip/pull/50
nmap        s   <Plug>(vsnip-select-text)
xmap        s   <Plug>(vsnip-select-text)
nmap        S   <Plug>(vsnip-cut-text)
xmap        S   <Plug>(vsnip-cut-text)

" If you want to use snippet for multiple filetypes, you can `g:vsnip_filetypes` for it.
let g:vsnip_filetypes = {}
let g:vsnip_filetypes.javascriptreact = ['javascript']
let g:vsnip_filetypes.typescriptreact = ['typescript']
let g:vsnip_snippet_dir = system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/snippets"')

"""""""""""""""""""""""""
"       Functions       "
"""""""""""""""""""""""""

function! SearchRange(type, ...)
	let sel_save = &selection
	let &selection = "inclusive"
	let reg_save = @@
	if a:0
		silent exe "normal! gvy"
	elseif a:type == 'line'
		silent exe "normal! '[V']y"
	else
		silent exe "normal! `[v`]y"
	endif
	exe "Ag" @@
	let &selection = sel_save
	let @@ = reg_save
endfunction

function! RemoveCurrentFileFromQuickfix()
	let curqfidx = getqflist({ 'idx': 1 }).idx - 1
	let qfall = getqflist()
	call remove(qfall, curqfidx)
	call setqflist(qfall, 'r')
	if len(qfall) == 0
		silent ccl
		return
	endif
endfunction

function! LinkZettel(val)
	let reg_save = @@
	let zid = system("ag -Qsl " . shellescape(a:val) . " | tr -d '\n'")
	silent exe "normal! gvy"
	silent exe "%s/".@@."/[".@@."]/"
	silent exe "normal! Go\<Esc>o[".@@."]: ./".zid."\<Esc>"
	let @@ = reg_save
endfunction

function! SetupZettelkasten()
	set textwidth=80
	nnoremap <silent> gl :exe "edit ".expand("**/".expand("<cword>")."**")<CR>
	vnoremap <silent> <Leader>l :<c-u>call fzf#run(fzf#wrap({'sink': funcref('LinkZettel')}))<CR>
endfunction

function! s:show_documentation()
	if (index(['vim','help'], &filetype) >= 0)
		exe 'h' expand('<cword>')
	endif
endfunction

"""""""""""""""""""""""
"      Mappings       "
"""""""""""""""""""""""

inoremap jk <Esc>

nnoremap S :%s//g<Left><Left>
nnoremap <silent> K :call <SID>show_documentation()<CR>

nmap <silent> ga <Plug>(EasyAlign)
nmap <silent> ga <Plug>(EasyAlign)
nmap <silent> ]g <Plug>(ale_next)
nmap <silent> [g <Plug>(ale_previous)
nmap <silent> gd <Plug>(ale_go_to_definition)
nmap <silent> ]h <Plug>(GitGutterNextHunk)
nmap <silent> [h <Plug>(GitGutterPrevHunk)
nmap <silent> <leader>hs <Plug>(GitGutterStageHunk)
nmap <silent> <leader>hu <Plug>(GitGutterUndoHunk)
nmap <silent> <leader>rn <Plug>(ale_rename)

nnoremap <silent> <Leader>w :w<CR>
nnoremap <silent> <Leader>q :q<CR>
nnoremap <silent> <Leader>g :G<CR>
nnoremap <silent> <Leader>j :sp<CR>
nnoremap <silent> <Leader>l :vsp<CR>

nnoremap <silent> <Leader>cd :call RemoveCurrentFileFromQuickfix()<CR>
nnoremap <silent> <Leader>yf :let @*=expand("%:p")<CR>

nnoremap <silent> <Leader>ec :e ~/.config/nvim/init.vim<CR>
nnoremap <silent> <Leader>so :so ~/.config/nvim/init.vim<CR>

nnoremap <silent> <Leader>=h :exe "resize +5"<CR>
nnoremap <silent> <Leader>-h :exe "resize -5"<CR>
nnoremap <silent> <Leader>=v :exe "vertical resize +5"<CR>
nnoremap <silent> <Leader>-v :exe "vertical resize -5"<CR>

"""""""""""""""""""""""""
"     Autocommands      "
"""""""""""""""""""""""""

au BufWritePost * GitGutter
au BufRead tmux.config set ft=tmux
au BufRead,BufNewFile *.req set ft=req
au BufRead,BufNewFile *.frag set ft=glsl
au BufRead,BufNewFile */zetz/*.md :call SetupZettelkasten()
au FileType gitcommit,gitrebase,gitconfig set bufhidden=delete

" Clear cmd line message
function! s:empty_message(timer)
  if mode() ==# 'n'
    echon ''
  endif
endfunction

augroup cmd_msg_cls
    autocmd!
    autocmd CmdlineLeave :  call timer_start(1500, funcref('s:empty_message'))
augroup END

"""""""""""""""""""""""""
"      Statusline       "
"""""""""""""""""""""""""

let g:statusline_mode_color_map = {
			\ 'i'  : '%3*',
			\ 'c'  : '%4*', 'cv' : '%4*', 'ce' : '%4*',
			\ 'v'  : '%5*',  'V'  : '%5*',  "\<C-V>" : '%5*',
			\ }

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
	let l:clear = '%*'
	let l:icon = a:active ? '(•◡•)' : '(ᴗ˳ᴗ)'
	let l:mode_color = a:active ? get(g:statusline_mode_color_map, mode(), '%1*') : '%2*'
	let l:filetype = empty(&filetype) ? '?' : '%{&filetype}'
	let l:fugitive = a:active && FugitiveHead() != '' ? '%3*●%* %{FugitiveHead()}' : ''
	let l:statusline_segments = [
				\ l:mode_color . '(%{g:statusline_mode_map[mode()]})' . l:clear,
				\ '   ',
				\ fnamemodify(getcwd(), ':t'),
				\ '   ',
				\ '%{expand("%")} %m %r %h',
				\ '%=',
				\ l:fugitive,
				\ '   ',
				\ l:filetype,
				\ '   ',
				\ l:icon 
				\]
	return join(l:statusline_segments)
endfunction

let s:is_statusline_visible = 1

function! ToggleStatusline()
	if s:is_statusline_visible == 1
		let s:is_statusline_visible = 0
		set noshowmode
		set noruler
		set laststatus=0
		set noshowcmd
	else
		let s:is_statusline_visible = 1
		set showmode
		set ruler
		set laststatus=2
		set showcmd
	endif
endfunction

" Switch between two statuslines: window focused / unfocused
augroup statusline_commands
    autocmd!
    autocmd WinEnter * setlocal statusline=%!StatusLine(1)
    autocmd WinLeave * setlocal statusline=%!StatusLine(0)
augroup END

" Setup <leader>hh to toggle statusline
nnoremap <silent> <leader>hh :call ToggleStatusline()<CR>

" Generate statusline
set statusline=%!StatusLine(1)

" Hide by default
call ToggleStatusline()

""""""""""""""""""""""""
"         FZF          "
""""""""""""""""""""""""

let g:fzf_layout = { 'down': '40%' }
let g:fzf_preview_window = ['right:50%:hidden', 'ctrl-]']

" Make :Ag not match file names, only the file content
command! -bang -nargs=* Ag call fzf#vim#ag(<q-args>, {'options': '--delimiter : --nth 4..'}, <bang>0)

nnoremap <silent> <leader>/ :Ag<CR>
nnoremap <silent> <leader>t :Tags<CR>
nnoremap <silent> <leader>f :Files<CR>
nnoremap <silent> <leader>. :GFiles<CR>
nnoremap <silent> <leader>b :Buffers<CR>
vnoremap <silent> <leader>/ :<c-u>call SearchRange(visualmode(), 1)<cr>

" Mapping selecting mappings
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

" Insert mode completion
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-l> <plug>(fzf-complete-line)

""""""""""""""""""""""""
"     Completion       "
""""""""""""""""""""""""

function! s:check_back_space() abort
	let col = col('.') - 1
	return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use tab to trigger completion or insert \t if there is whitespace.
inoremap <silent><expr> <TAB>
            \ pumvisible() ? "\<C-n>" :
            \ <SID>check_back_space() ? "\<TAB>" :
            \ "\<C-x>\<C-i>"
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

" Use <cr> to confirm completion and `<C-g>u` to break undo chain at current position.
if exists('*complete_info')
    inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
    inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

