" Sourced by kitty's scrollback_pager (kitty.conf), which runs with -u NONE
" so none of vimrc's plugins or the vim-oscyank TextYankPost autocmd load.
" This replicates just the yank-to-system-clipboard piece for that context.
source ~/.vim/pack/bundle/start/vim-oscyank/plugin/oscyank.vim

augroup scrollback_pager_oscyank
  autocmd!
  autocmd TextYankPost * if v:event.operator ==# 'y' | call OSCYankRegister('"') | endif
augroup END

nnoremap q ZQ
