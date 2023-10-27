vim9script

import autoload 'utils.vim'

augroup VimspectorUICustomisation
  autocmd!
  autocmd User VimspectorUICreated      utils.VimspectorCustomiseUI()
  autocmd User VimspectorTerminalOpened utils.VimspectorSetUpTerminal()
augroup END
