vim9script

export def CustomiseUI(): void
  var wins = g:vimspector_session_windows
  win_gotoid(wins.variables)
  wincmd H
  nmap <silent> <buffer> <LocalLeader>di <Plug>VimspectorBalloonEval
  win_gotoid(wins.watches)
  nmap <silent> <buffer> <LocalLeader>di <Plug>VimspectorBalloonEval
  win_gotoid(wins.code)
  wincmd K
  :25wincmd _
enddef

export def CustomiseTerminal(): void
  var wins = g:vimspector_session_windows
  var console_buf_names = filter(map(range(bufnr('$')), 'bufname(v:val)'), 'v:val =~ "Console"')
  var console_buf_win_id = win_findbuf(bufnr(console_buf_names[0]))
  if empty(console_buf_win_id) == 0
    win_execute(console_buf_win_id[0], 'resize 5')
  endif
  if bufexists(winbufnr(wins.terminal))
    win_execute(wins.terminal, 'hide')
  endif
  if bufexists(winbufnr(wins.output))
    win_execute(wins.output, 'hide')
  endif
enddef
