function! s:yank_absolute_file_path()
  let l:file = expand('%:p')
  if l:file == ''
    echo "No file!"
    return
  endif
  let @+ = l:file
  echomsg "Yanked: " . l:file
endfunction

function! s:yank_relative_file_path()
  let l:root = getcwd()
  let l:file = expand('%:p')
  if l:file == ''
    echo "No file!"
    return
  endif
  let l:relative_path = substitute(l:file, l:root . '/', '', '')
  let @+ = l:relative_path
  echomsg "Yanked: " . l:relative_path
endfunction

function! s:yank_current_date()
  let l:date = strftime('%Y-%m-%d')
  let @+ = l:date
  echomsg "Yanked: " . l:date
endfunction

function! s:yank_file_location()
  let l:file = expand('%:p')
  if l:file == ''
    echo "No file!"
    return
  endif
  let l:location = l:file . ':' . line('.') . ':' . col('.')
  let @+ = l:location
  echomsg "Yanked: " . l:location
endfunction

function! s:yank_line_with_location(l1, l2)
  let l:file = expand('%:p')
  if l:file == ''
    echo "No file!"
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

command! -bar -nargs=0 YankCurrentDate      call <sid>yank_current_date()
command! -bar -nargs=0 YankRelativeFilePath call <sid>yank_relative_file_path()
command! -bar -nargs=0 YankAbsoluteFilePath call <sid>yank_absolute_file_path()
command! -bar -nargs=0 YankFileLocation     call <sid>yank_file_location()
command! -bar -range   YankLineWithLocation call <sid>yank_line_with_location(<line1>, <line2>)
