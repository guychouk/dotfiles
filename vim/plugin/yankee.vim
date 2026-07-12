function! s:YankAbsoluteFilePath()
  let l:file = expand('%:p')
  if l:file == ''
    echo "No file associated with this buffer"
    return
  endif
  let @+ = l:file
  echomsg "Yanked: " . l:file
endfunction

function! s:YankRelativeFilePath()
  let l:root = getcwd()
  let l:file = expand('%:p')
  if l:file == ''
    echo "No file associated with this buffer"
    return
  endif
  let l:relative_path = substitute(l:file, l:root . '/', '', '')
  let @+ = l:relative_path
  echomsg "Yanked: " . l:relative_path
endfunction

function! s:YankCurrentDate()
  let l:date = strftime('%Y-%m-%d')
  let @+ = l:date
  echomsg "Yanked: " . l:date
endfunction

function! s:YankFileLocation()
  let l:file = expand('%:p')
  if l:file == ''
    echo "No file associated with this buffer"
    return
  endif
  let l:location = l:file . ':' . line('.') . ':' . col('.')
  let @+ = l:location
  echomsg "Yanked: " . l:location
endfunction

function! s:YankLineWithLocation(l1, l2)
  let l:file = expand('%:p')
  if l:file == ''
    echo "No file associated with this buffer"
    return
  endif
  let l:lines = getline(a:l1, a:l2)
  if a:l1 == a:l2
    let l:location = l:file . ':' . a:l1 . ':' . col('.')
  else
    let l:location = l:file . ':' . a:l1 . '-' . a:l2
  endif
  let @+ = l:location . "\n" . join(l:lines, "\n")
  echomsg "Yanked: " . l:location
endfunction

command! -bar -nargs=0 YankCurrentDate      call <sid>YankCurrentDate()
command! -bar -nargs=0 YankRelativeFilePath call <sid>YankRelativeFilePath()
command! -bar -nargs=0 YankAbsoluteFilePath call <sid>YankAbsoluteFilePath()
command! -bar -nargs=0 YankFileLocation     call <sid>YankFileLocation()
command! -bar -range   YankLineWithLocation call <sid>YankLineWithLocation(<line1>, <line2>)
