vim9script

import autoload 'microline.vim'

def SetupMicrolineColors()
  var sl_bg = synIDattr(synIDtrans(hlID('StatusLine')), 'fg', 'gui')
  var sl_fg = synIDattr(synIDtrans(hlID('StatusLine')), 'bg', 'gui')
  if has('gui_running')
    sl_bg = synIDattr(synIDtrans(hlID('StatusLine')), 'bg', 'gui')
    sl_fg = synIDattr(synIDtrans(hlID('StatusLine')), 'fg', 'gui')
  endif
  var sl_nc_bg = synIDattr(synIDtrans(hlID('StatusLineNC')), 'bg', 'gui')
  var sl_nc_fg = synIDattr(synIDtrans(hlID('StatusLineNC')), 'fg', 'gui')
  exe "highlight MicrolineLeftSep "  .. "guibg=" .. sl_bg    .. " guifg=" .. sl_nc_bg
  exe "highlight MicrolineRightSep " .. "guibg=" .. sl_nc_bg .. " guifg=" .. sl_bg
  exe "highlight MicrolineSection "  .. "guibg=" .. sl_nc_bg .. " guifg=" .. sl_nc_fg
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
