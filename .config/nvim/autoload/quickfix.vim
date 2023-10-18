function! quickfix#ReadFileToQuickfix(filepath)
  let l:qf_list = []

  for line in readfile(a:filepath)
    let l:parts = split(line, ':')
    if len(l:parts) >= 3
      let l:filename = l:parts[0]
      let l:lnum = l:parts[1]
      let l:col = l:parts[2]
      let l:message = len(l:parts) >= 4 ? l:parts[3] : ''
      call add(l:qf_list, {'filename': l:filename, 'lnum': l:lnum, 'col': l:col, 'text': l:message})
    endif
  endfor

  call setqflist(l:qf_list)
  copen
endfunction

