" Git helpers

function! s:GitUnstagedToQuickfix()
  let l:root = getcwd()
  let l:diff = systemlist('git diff -U0')
  let l:qflist = []
  let l:current_file = ''
  let l:current_lnum = 0
  let l:in_hunk = 0
  for l:line in l:diff
    if l:line =~# '^diff --git'
      let l:in_hunk = 0
      continue
    elseif l:line =~# '^+++ b/'
      let l:current_file = l:root . '/' . substitute(l:line, '^+++ b/', '', '')
      let l:in_hunk = 0
    elseif l:line =~# '^@@'
      let l:match = matchlist(l:line, '^@@ -\d\+\(,\d\+\)\? +\(\d\+\)')
      if !empty(l:match)
        let l:current_lnum = str2nr(l:match[2])
        let l:in_hunk = 1
      endif
    elseif l:in_hunk && (l:line =~# '^+' || l:line =~# '^-')
      let l:text = substitute(l:line, '^[+-]', '', '')
      call add(l:qflist, {
        \ 'filename': l:current_file,
        \ 'lnum': l:current_lnum,
        \ 'text': l:text
        \ })
      let l:in_hunk = 0
    endif
  endfor
  call setqflist(l:qflist)
  copen
endfunction

function! s:GitCheckoutBranch(branch)
  " Remove leading/trailing whitespace and any * marker
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
  " Execute checkout
  let l:output = system(l:cmd . ' 2>&1')
  if v:shell_error != 0
    echohl ErrorMsg
    echom "Git checkout failed: " . substitute(trim(l:output), '\n', ' ', 'g')
    echohl None
    return
  endif
  if expand('%') != ''
    silent! edit
  endif
  echom "Switched to: " . l:branch
  " Trigger any git-related plugins to update
  silent! doautocmd User FugitiveChanged
endfunction

function! s:GitSwitchBranch()
  " Get all branches (local and remote), remove HEAD entries
  let l:source_cmd = 'git branch -a | grep -v HEAD | sed "s/^[* ]*//" | sed "s#remotes/##"'
  let l:preview_cmd = 'git log --oneline --graph --date=short --pretty="format:%C(auto)%cd %h%d %s" $(echo {} | sed "s#origin/##") --color=always | head -50'
  call fzf#run(fzf#wrap({
        \ 'source': l:source_cmd,
        \ 'sink': function('s:GitCheckoutBranch'),
        \ 'options': ['--prompt=🪵 ', '--preview', l:preview_cmd]
        \ }))
endfunction

command! -bar -nargs=0 Branches               call <sid>GitSwitchBranch()
command! -bar -nargs=0 GitUnstagedToQuickfix  call <sid>GitUnstagedToQuickfix()
