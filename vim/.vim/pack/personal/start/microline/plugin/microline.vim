vim9script

import autoload 'microline.vim'

def SetupMicrolineColors()
  var sl_bg = synIDattr(synIDtrans(hlID('StatusLine')), 'bg', 'gui')
  var sl_fg = synIDattr(synIDtrans(hlID('StatusLine')), 'fg', 'gui')
  var sl_nc_bg = synIDattr(synIDtrans(hlID('StatusLineNC')), 'bg', 'gui')
  var sl_nc_fg = synIDattr(synIDtrans(hlID('StatusLineNC')), 'fg', 'gui')
  exe "highlight MicrolineSep " .. "guibg=" .. sl_bg .. " guifg=" .. sl_nc_bg
  exe "highlight MicrolineSection " .. "guibg=" .. sl_nc_bg .. " guifg=" .. sl_nc_fg
enddef

augroup Microline
  autocmd!
  autocmd SigUSR1            *  SetupMicrolineColors()
  autocmd ColorScheme        *  SetupMicrolineColors()
  autocmd WinEnter,BufEnter  *  setlocal statusline=%!microline#Build(v:true)
  autocmd WinLeave,BufLeave  *  setlocal statusline=%!microline#Build(v:false)
  autocmd CmdWinEnter        *  setlocal statusline=\ Command\ Line\ %1*
augroup END

command! MicrolineToggle microline.Toggle()

SetupMicrolineColors()
