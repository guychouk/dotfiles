function! s:GithubBrowse() range abort
  let l:branch = substitute(system('git rev-parse --abbrev-ref HEAD'), '\n', '', 'g')
  let l:file = substitute(system('git ls-files --full-name ' . expand('%:p')), '\n', '', 'g')
  let l:range = a:firstline == a:lastline
        \ ? l:file . ':' . a:firstline
        \ : l:file . ':' . a:firstline . '-' . a:lastline
  let l:url = substitute(system('gh browse -n --branch ' . shellescape(l:branch) . ' ' . shellescape(l:range)), '\n', '', 'g')
  let @+ = l:url
endfunction

function! s:GitDiff(...) abort
  if a:0 > 0 && !empty(a:1)
    let l:args = a:1
  else
    call system('git rev-parse --verify main 2>/dev/null')
    let l:args = (v:shell_error == 0 ? 'main' : 'master') . '...HEAD'
  endif
  let l:base = trim(system('git merge-base ' . split(l:args, '\.\.\.')[0] . ' HEAD'))
  let g:gitgutter_diff_base = l:base
  call setqflist(map(systemlist('git diff --name-only --relative ' . l:args), {_, f -> {'filename': f, 'lnum': 1}}), 'r')
  copen
  GitGutterAll
endfunction

command! -range           GithubBrowse <line1>,<line2>call <sid>GithubBrowse()
command! -bar    -nargs=? GitDiff      call <sid>GitDiff(<f-args>)
command! -bar    -nargs=0 GitDiffClear unlet! g:gitgutter_diff_base | GitGutterAll
