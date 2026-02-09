function! s:RemoveQfItem() abort
  let curqfidx = line('.') - 1
  let qfall = getqflist()
  call remove(qfall, curqfidx)
  call setqflist(qfall, 'r')
  execute curqfidx + 1 . "cfirst"
  :copen
endfunction

function! s:PopulateQfWithFiles(pattern) abort
  let files = glob(a:pattern, 0, 1)
  if empty(files)
    echohl WarningMsg | echomsg 'No files found' | echohl None
    return
  endif
  call setqflist(map(files, {_, val -> {'filename': val}}))
  copen
endfunction

function! s:PopulateLocWithFiles(pattern) abort
  let files = glob(a:pattern, 0, 1)
  if empty(files)
    echohl WarningMsg | echomsg 'No files found' | echohl None
    return
  endif
  call setloclist(0, map(files, {_, val -> {'filename': val}}))
  lopen
endfunction

command! -bar     -nargs=0           RemoveQfItem   call <sid>RemoveQfItem()
command! -nargs=1 -complete=file     Qfiles         call <sid>PopulateQfWithFiles(<q-args>)
command! -nargs=1 -complete=file     Lfiles         call <sid>PopulateLocWithFiles(<q-args>)
