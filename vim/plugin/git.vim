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

function! s:ReviewBranch(...) abort
  if a:0 > 0 && !empty(a:1)
    let l:base = a:1
  else
    call system('git rev-parse --verify main 2>/dev/null')
    let l:base = v:shell_error == 0 ? 'main' : 'master'
  endif
  let l:files = systemlist('git diff --name-status ' . shellescape(l:base) . '...HEAD')
  if v:shell_error != 0
    echohl ErrorMsg | echom 'Failed to get changed files' | echohl None
    return
  endif
  if empty(l:files)
    echohl WarningMsg | echom 'No files changed compared to ' . l:base | echohl None
    return
  endif
  let l:qf_list = []
  for l:line in l:files
    let l:parts = split(l:line, '\t')
    if len(l:parts) >= 2
      call add(l:qf_list, {'filename': l:parts[1], 'lnum': 1, 'text': l:parts[0]})
    endif
  endfor
  call setqflist(l:qf_list, 'r')
  call setqflist([], 'a', {'title': 'Review vs ' . l:base})
  copen
endfunction

function! s:PRComment() range abort
  let l:current_file = expand('%')
  if l:current_file =~# '^fugitive://'
    let l:real_path = FugitiveReal(l:current_file)
    let l:side = 'LEFT'
  else
    let l:real_path = expand('%:p')
    let l:side = 'RIGHT'
  endif
  let l:commit_sha = substitute(system('git rev-parse HEAD'), '\n', '', 'g')
  if v:shell_error != 0
    echohl ErrorMsg | echom 'Could not get commit SHA' | echohl None
    return
  endif
  let l:file = substitute(system('git ls-files --full-name ' . shellescape(l:real_path)), '\n', '', 'g')
  if v:shell_error != 0 || empty(l:file)
    echohl ErrorMsg | echom 'Could not get repo-relative path' | echohl None
    return
  endif
  let l:pr_number = substitute(system('gh pr view --json number -q .number 2>/dev/null'), '\n', '', 'g')
  if v:shell_error != 0
    echohl ErrorMsg | echom 'No open PR for current branch' | echohl None
    return
  endif
  let l:repo = substitute(system('gh repo view --json nameWithOwner -q .nameWithOwner'), '\n', '', 'g')
  if v:shell_error != 0
    echohl ErrorMsg | echom 'Could not get repo info' | echohl None
    return
  endif
  let l:side_label = l:side ==# 'LEFT' ? ' (OLD)' : ' (NEW)'
  let l:temp_file = tempname()
  call writefile([
        \ '# PR #' . l:pr_number . ' comment for ' . l:file . ':' . a:firstline . (a:firstline != a:lastline ? '-' . a:lastline : '') . l:side_label,
        \ '# Lines starting with # will be ignored',
        \ ''
        \ ], l:temp_file)
  let s:pr_comment_context = {
        \ 'file': l:file, 'start_line': a:firstline, 'end_line': a:lastline,
        \ 'pr_number': l:pr_number, 'commit_sha': l:commit_sha,
        \ 'repo': l:repo, 'side': l:side
        \ }
  execute 'split ' . l:temp_file
  setlocal buftype=acwrite bufhidden=wipe
  autocmd! BufWriteCmd <buffer> call <SID>SubmitPRComment()
endfunction

function! s:SubmitPRComment() abort
  let l:lines = filter(getline(1, '$'), 'v:val !~# "^#"')
  let l:body = substitute(join(l:lines, "\n"), '^\s*\|\s*$', '', 'g')
  if empty(l:body)
    echohl WarningMsg | echom 'Empty comment, not submitting' | echohl None
    set nomodified
    return
  endif
  let l:ctx = s:pr_comment_context
  let l:payload = {
        \ 'body': l:body, 'path': l:ctx.file,
        \ 'commit_id': l:ctx.commit_sha, 'side': l:ctx.side,
        \ 'line': l:ctx.end_line
        \ }
  if l:ctx.start_line != l:ctx.end_line
    let l:payload['start_line'] = l:ctx.start_line
    let l:payload['start_side'] = l:ctx.side
  endif
  let l:payload_file = tempname()
  call writefile([json_encode(l:payload)], l:payload_file)
  let l:result = system('gh api repos/' . l:ctx.repo . '/pulls/' . l:ctx.pr_number . '/comments -X POST --input ' . shellescape(l:payload_file) . ' 2>&1')
  if v:shell_error != 0
    echohl ErrorMsg | echom 'Failed: ' . substitute(l:result, '\n', ' ', 'g') | echohl None
    return
  endif
  call delete(l:payload_file)
  set nomodified
  echom 'Comment posted'
  quit
endfunction

vnoremap <silent> <leader>pc :call <SID>PRComment()<CR>

command! -bar    -nargs=0 Branches     call <sid>GitSwitchBranch()
command! -range           GithubBrowse <line1>,<line2>call <sid>GithubBrowse()
command! -bar    -nargs=? ReviewBranch call <sid>ReviewBranch(<f-args>)
