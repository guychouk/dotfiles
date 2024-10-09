vim9script

import autoload 'picoline.vim'

def SetupPicolineColors()
  var sl_bg = synIDattr(synIDtrans(hlID('StatusLine')), 'fg', 'gui')
  var sl_fg = synIDattr(synIDtrans(hlID('StatusLine')), 'bg', 'gui')
  if has('gui_running')
    sl_bg = synIDattr(synIDtrans(hlID('StatusLine')), 'bg', 'gui')
    sl_fg = synIDattr(synIDtrans(hlID('StatusLine')), 'fg', 'gui')
  endif
  var sl_nc_bg = synIDattr(synIDtrans(hlID('StatusLineNC')), 'bg', 'gui')
  var sl_nc_fg = synIDattr(synIDtrans(hlID('StatusLineNC')), 'fg', 'gui')
  exe "highlight PicolineLeftSep "  .. "guibg=" .. sl_bg    .. " guifg=" .. sl_nc_bg
  exe "highlight PicolineRightSep " .. "guibg=" .. sl_nc_bg .. " guifg=" .. sl_bg
  exe "highlight PicolineSection "  .. "guibg=" .. sl_nc_bg .. " guifg=" .. sl_nc_fg
enddef

augroup Picoline
  autocmd!
  autocmd SigUSR1            *  SetupPicolineColors()
  autocmd ColorScheme        *  SetupPicolineColors()
  autocmd WinEnter,BufEnter  *  setlocal statusline=%!picoline#Build(v:true)
  autocmd WinLeave,BufLeave  *  setlocal statusline=%!picoline#Build(v:false)
  autocmd CmdWinEnter        *  setlocal statusline=\ Command\ Line\ %1*
augroup END

command! PicolineToggle picoline.Toggle()

SetupPicolineColors()
