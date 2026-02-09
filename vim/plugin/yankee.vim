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

command! -bar -nargs=0 YankCurrentDate      call <sid>YankCurrentDate()
command! -bar -nargs=0 YankRelativeFilePath call <sid>YankRelativeFilePath()
command! -bar -nargs=0 YankAbsoluteFilePath call <sid>YankAbsoluteFilePath()
