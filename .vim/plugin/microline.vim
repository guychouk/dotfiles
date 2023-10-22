vim9script

import autoload 'microline.vim'

highlight MicrolineSep     guibg=#2C323C guifg=#1E1E1E
highlight MicrolineSection guibg=#1E1E1E guifg=NONE

augroup Microline
	autocmd!
	autocmd VimEnter * setlocal statusline=%!microline#Build(1)
	autocmd WinEnter * setlocal statusline=%!microline#Build(1)
	autocmd WinLeave * setlocal statusline=%!microline#Build(0)

augroup END

command! MicrolineToggle microline#Toggle()
