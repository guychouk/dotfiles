function! s:DefaultBranch() abort
  let l:ref = trim(system('git symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null'))
  return !empty(l:ref) ? l:ref : 'main'
endfunction

function! s:GitReview(...) abort
  let l:args = (a:0 > 0 && !empty(a:1)) ? a:1 : s:DefaultBranch() . '...HEAD'
  let l:base = trim(system('git merge-base ' . split(l:args, '\.\.\.')[0] . ' HEAD'))
  let g:gitgutter_diff_base = l:base

  " One quickfix entry per hunk (lnum = new-side start line), so [q/]q alone
  " covers what used to need [c/]c inside a file too. Files with no hunks
  " (renames, mode-only changes, binaries) still get a lnum:1 fallback entry.
  let l:qf = []
  let l:hunked = {}
  let l:path = ''
  for l:line in systemlist('git diff --unified=0 --relative ' . l:args)
    let l:m = matchlist(l:line, '^+++ b/\(.*\)$')
    if !empty(l:m)
      let l:path = l:m[1]
      continue
    endif
    let l:m = matchlist(l:line, '^@@ -\d\+\%(,\d\+\)\? +\(\d\+\)')
    if !empty(l:m) && !empty(l:path)
      call add(l:qf, {'filename': l:path, 'lnum': max([str2nr(l:m[1]), 1])})
      let l:hunked[l:path] = 1
    endif
  endfor
  for l:f in systemlist('git diff --name-only --relative ' . l:args)
    if !has_key(l:hunked, l:f)
      call add(l:qf, {'filename': l:f, 'lnum': 1})
    endif
  endfor

  call setqflist(l:qf, 'r')
  copen
  GitGutterAll
endfunction

command! -bar -nargs=? GitReview      call <sid>GitReview(<f-args>)
command! -bar -nargs=0 GitReviewClear unlet! g:gitgutter_diff_base | GitGutterAll
