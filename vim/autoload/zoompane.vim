vim9script

export def Toggle(): void
  if exists('t:zoomed') && t:zoomed
    execute t:zoom_winrestcmd
    t:zoomed = 0
  else
    t:zoom_winrestcmd = winrestcmd()
    resize
    vertical resize
    t:zoomed = 1
  endif
enddef
