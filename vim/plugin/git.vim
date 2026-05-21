function! s:DefaultBranch() abort
  let l:ref = trim(system('git symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null'))
  return !empty(l:ref) ? l:ref : 'main'
endfunction

function! s:GitReview(...) abort
  let l:args = (a:0 > 0 && !empty(a:1)) ? a:1 : s:DefaultBranch() . '...HEAD'
  let l:base = trim(system('git merge-base ' . split(l:args, '\.\.\.')[0] . ' HEAD'))
  let g:gitgutter_diff_base = l:base
  call setqflist(map(systemlist('git diff --name-only --relative ' . l:args), {_, f -> {'filename': f, 'lnum': 1}}), 'r')
  copen
  GitGutterAll
endfunction

command! -bar -nargs=? GitReview      call <sid>GitReview(<f-args>)
command! -bar -nargs=0 GitReviewClear unlet! g:gitgutter_diff_base | GitGutterAll
