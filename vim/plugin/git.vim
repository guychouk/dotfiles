function! s:GitReview(...) abort
  let l:range = (a:0 > 0 && !empty(a:1)) ? a:1 : ''
  let l:base = trim(system('cr base ' . l:range))
  if v:shell_error
    echoerr 'cr base failed: ' . l:base
    return
  endif
  let g:gitgutter_diff_base = l:base

  let l:qf = []
  for l:line in systemlist('cr hunks ' . l:range)
    let l:m = matchlist(l:line, '^\(.*\):\(\d\+\)$')
    if !empty(l:m)
      call add(l:qf, {'filename': l:m[1], 'lnum': str2nr(l:m[2])})
    endif
  endfor
  call setqflist(l:qf, 'r')
  copen
  GitGutterAll
endfunction

command! -bar -nargs=? GitReview      call <sid>GitReview(<f-args>)
command! -bar -nargs=0 GitReviewClear unlet! g:gitgutter_diff_base | GitGutterAll
