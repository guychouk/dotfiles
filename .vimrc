
"""""""""""""""""""""""""
"         Plug          "
"""""""""""""""""""""""""

"call plug#begin(split(&rtp, ',')[0] . '/plugins')
"call plug#end()


"""""""""""""""""""""""""
"        Options        "
"""""""""""""""""""""""""
filetype plugin indent on  " Sets on filetype (Also from plugins)
syntax on                  " Sets on syntax highlighting
set tabstop=4              " Sets tab to 4 instead of vim's crazy 8
set backspace=2 	       " Fix <BS> key
set laststatus=2           " Set statusbar to always appear (for Lightline)
set shiftwidth=4           " Assits with code formatting
set encoding=utf-8	       " Encoding for files
set clipboard=unnamed      " Mainly for better compatibility with Windows
set foldmethod=syntax      " Set foldmethod for programming
set nowrap                 " Disable line wrapping
set smarttab               " Improves tabbing
set incsearch              " Sets incremental search
set noshowmode             " Disable showing of current VIM mode (for Lightline)
set autoindent             " New lines will be indented as well
set smartcase              " No ignore case when pattern has uppercase
set nohlsearch             " Disable search highlight
set noswapfile             " Disables swp files creation
set nocompatible           " Required for ViM to be iMproved, turn off for vi-like usage


"""""""""""""""""""""""""
"       Variables       "
"""""""""""""""""""""""""
let mapleader = " "
let maplocalleader = " "


"""""""""""""""""""""""""
"        Remaps         "
"""""""""""""""""""""""""
imap jj <Esc>
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
nnoremap <Leader>w :w<CR>
nnoremap <Leader>q :q<CR>
nnoremap <Leader>d "_d
nnoremap <Leader>ec :vsp $DRIVE_ETC/.vimrc<CR>
nnoremap <Leader>ba :vsp ~/.bash_aliases<CR>
nnoremap <Leader>ps :vsp $DRIVE_ETC/profile.ps1<CR>
nnoremap <Leader>so :so ~/_vimrc<CR>
nnoremap <Leader>pi :PlugInstall<CR>
nnoremap <Leader>pc :PlugClean<CR>
nnoremap <Leader>1 :vsp $DRIVE_ETC/.vimrc<CR>:Explore<CR>
nnoremap <Leader>c :%s!"!!g<CR>:%s! = !=!g<CR>:%s!\n!\&!g<CR>
nnoremap <Leader><Leader> :ZoomToggle<CR>


"""""""""""""""""""""""""
"       Functions       "
"""""""""""""""""""""""""

" Zoom / Restore window.
function! s:ZoomToggle() abort
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
command! ZoomToggle call s:ZoomToggle()


"""""""""""""""""""""""""
"        Plugins        "
"""""""""""""""""""""""""

" CtrlP
set wildignore+=*\\tmp\\*,*.swp,*.zip,*.exe

let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll)$',
  \ }
