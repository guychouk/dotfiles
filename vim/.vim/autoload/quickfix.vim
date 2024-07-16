vim9script

export def AddOpenBuffersToList(): void
  var qf_list = map(filter(range(1, bufnr('$')), 'buflisted(v:val)'), '{"bufnr": v:val}')
  setqflist(qf_list)
  copen
enddef

export def RemoveItem(): void
  var qf_list = getqflist()
  if len(qf_list) <= 1
    cclose
    return
  endif
  var curqfidx = line('.') - 1
  var nextqfidx = curqfidx + 1
  call remove(qf_list, curqfidx)
  call setqflist(qf_list, 'r')
  execute ':' .. nextqfidx .. 'cfirst'
  copen
enddef

export def ReadListFromFile(filepath: string): void
  var qf_list = []
  for line in readfile(filepath)
    var parts = split(line, ':')
    if len(parts) >= 3
      var filename = parts[0]
      var lnum = str2nr(parts[1])
      var col = str2nr(parts[2])
      var message = len(parts) >= 4 ? parts[3] : ''
      call add(qf_list, {'filename': filename, 'lnum': lnum, 'col': col, 'text': message})
    endif
  endfor
  call setqflist(qf_list)
  copen
enddef
