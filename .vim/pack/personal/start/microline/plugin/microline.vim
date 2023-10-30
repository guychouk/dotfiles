vim9script

import autoload 'microline.vim'

def SetupMicrolineColors()
  var sl_fg = synIDattr(synIDtrans(hlID('StatusLine')), 'fg', 'gui')
  var sl_nc_bg = synIDattr(synIDtrans(hlID('StatusLineNC')), 'bg', 'gui')
  exe "highlight MicrolineSep " .. "guibg=" .. sl_fg .. " guifg=" .. sl_nc_bg
  exe "highlight MicrolineSection " .. "guibg=" .. sl_nc_bg .. " guifg=" .. sl_fg
enddef

augroup Microline
  autocmd!
  autocmd ColorScheme        *  SetupMicrolineColors()
  autocmd WinEnter,BufEnter  *  setlocal statusline=%!microline#Build(v:true)
  autocmd WinLeave,BufLeave  *  setlocal statusline=%!microline#Build(v:false)
  autocmd CmdWinEnter        *  setlocal statusline=\ Command\ Line\ %1*
augroup END

command! MicrolineToggle microline.Toggle()

SetupMicrolineColors()
