function! s:GitCheckoutBranch(branch) abort
  let l:branch = substitute(trim(a:branch), '^\*\s*', '', '')
  let l:branch = trim(l:branch)
  if empty(l:branch)
    return
  endif
  if l:branch =~# '^origin/'
    let l:local_branch = substitute(l:branch, '^origin/', '', '')
    call system('git rev-parse --verify ' . shellescape(l:local_branch) . ' 2>/dev/null')
    if v:shell_error == 0
      let l:cmd = 'git checkout ' . shellescape(l:local_branch)
    else
      let l:cmd = 'git checkout -b ' . shellescape(l:local_branch) . ' --track ' . shellescape(l:branch)
    endif
  else
    let l:cmd = 'git checkout ' . shellescape(l:branch)
  endif
  let l:output = system(l:cmd . ' 2>&1')
  if v:shell_error != 0
    echohl ErrorMsg
    echom 'Git checkout failed: ' . substitute(trim(l:output), '\n', ' ', 'g')
    echohl None
    return
  endif
  if expand('%') != ''
    silent! edit
  endif
  echom 'Switched to: ' . l:branch
  silent! doautocmd User FugitiveChanged
endfunction

function! s:GitSwitchBranch() abort
  let l:source_cmd = 'git branch -a | grep -v HEAD | sed "s/^[* ]*//" | sed "s#remotes/##"'
  let l:preview_cmd = 'git log --oneline --graph --date=short --pretty="format:%C(auto)%cd %h%d %s" $(echo {} | sed "s#origin/##") --color=always | head -50'
  call fzf#run(fzf#wrap({
        \ 'source': l:source_cmd,
        \ 'sink': function('s:GitCheckoutBranch'),
        \ 'options': ['--prompt', 'branch> ', '--preview', l:preview_cmd]
        \ }))
endfunction

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
  call setqflist(map(systemlist('git diff --name-only --relative ' . l:args), {_, f -> {'filename': f, 'lnum': 1}}), 'r')
  copen
endfunction

command! -nargs=1         Browse silent call system('open ' . shellescape(<q-args>))
command! -bar    -nargs=0 Branches     call <sid>GitSwitchBranch()
command! -range           GithubBrowse <line1>,<line2>call <sid>GithubBrowse()
command! -bar    -nargs=? GitDiff      call <sid>GitDiff(<f-args>)
