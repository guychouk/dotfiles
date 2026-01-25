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

function s:GithubBrowse() range
  let l:current_branch = substitute(system('git rev-parse --abbrev-ref HEAD'), '\n', '', 'g')
  let l:relative_current_file = substitute(system('git ls-files --full-name ' . expand('%:p')), '\n', '', 'g')
  if a:firstline == a:lastline
    let l:gh_url = substitute(system('gh browse -n --branch ' . shellescape(l:current_branch) . ' ' . shellescape(l:relative_current_file . ':' . a:firstline)), '\n', '', 'g')
  else
    let l:gh_url = substitute(system('gh browse -n --branch ' . shellescape(l:current_branch) . ' ' . shellescape(l:relative_current_file . ':' . a:firstline . '-' . a:lastline)), '\n', '', 'g')
  endif
  let @+ = l:gh_url
endfunction

" Load changed files from current branch into quickfix for code review
function! s:ReviewBranch(...)
  " Use provided base branch or auto-detect (main/master)
  if a:0 > 0 && !empty(a:1)
    let l:base = a:1
  else
    " Try to detect main or master branch
    call system('git rev-parse --verify main 2>/dev/null')
    let l:base = v:shell_error == 0 ? 'main' : 'master'
  endif

  " Store base branch for later use
  let g:review_base_branch = l:base

  " Get list of changed files compared to base branch
  let l:files = systemlist('git diff --name-status ' . shellescape(l:base) . '...HEAD')

  if v:shell_error != 0
    echohl ErrorMsg
    echom 'Error: Failed to get changed files'
    echohl None
    return
  endif

  if empty(l:files)
    echohl WarningMsg
    echom 'No files changed compared to ' . l:base
    echohl None
    return
  endif

  " Parse git diff output (format: "M\tfilename" or "A\tfilename")
  " and create quickfix entries
  let l:qf_list = []
  for l:line in l:files
    let l:parts = split(l:line, '\t')
    if len(l:parts) >= 2
      let l:status = l:parts[0]
      let l:file = l:parts[1]
      let l:status_text = l:status ==# 'M' ? 'Modified' :
                        \ l:status ==# 'A' ? 'Added' :
                        \ l:status ==# 'D' ? 'Deleted' :
                        \ l:status ==# 'R' ? 'Renamed' :
                        \ 'Changed'
      call add(l:qf_list, {
            \ 'filename': l:file,
            \ 'lnum': 1,
            \ 'text': l:status_text
            \ })
    endif
  endfor

  " Populate quickfix list
  call setqflist(l:qf_list, 'r')
  call setqflist([], 'a', {'title': 'Branch Review: ' . system('git rev-parse --abbrev-ref HEAD')[:-2] . ' vs ' . l:base})

  " Open quickfix window
  copen

  " Set up mapping to open files with Gvdiffsplit
  nnoremap <buffer> <CR> :call <SID>ReviewOpenFile()<CR>

  " Enable review mode mappings
  call s:EnableReviewMappings()

  echom 'Loaded ' . len(l:qf_list) . ' file(s) into quickfix. Use ]q/[q to navigate with diffs.'
endfunction

" Open file from quickfix with Gvdiffsplit against base branch
function! s:ReviewOpenFile()
  if !exists('g:review_base_branch')
    " Fallback to normal behavior if no review session
    execute "normal! \<CR>"
    return
  endif

  " Get the current quickfix item
  let l:qf_idx = line('.')
  let l:qf_item = getqflist()[l:qf_idx - 1]

  " Get the filename from the quickfix item
  let l:filename = bufname(l:qf_item.bufnr)

  " Close quickfix and open the file
  wincmd p
  execute 'edit ' . fnameescape(l:filename)

  " Open Ghdiffsplit against base branch
  execute 'Ghdiffsplit ' . g:review_base_branch
endfunction

" Enable/disable review mode mappings
function! s:EnableReviewMappings()
  nnoremap <silent> ]q :call <SID>ReviewNext()<CR>
  nnoremap <silent> [q :call <SID>ReviewPrev()<CR>
  let g:review_mappings_enabled = 1

  " Set up autocommand to disable when quickfix list changes
  augroup ReviewMode
    autocmd!
    autocmd QuickFixCmdPost * call s:CheckReviewMode()
  augroup END
endfunction

function! s:DisableReviewMappings()
  if exists('g:review_mappings_enabled') && g:review_mappings_enabled
    silent! nunmap ]q
    silent! nunmap [q
    unlet g:review_mappings_enabled
    if exists('g:review_base_branch')
      unlet g:review_base_branch
    endif

    " Clean up autocommand
    augroup ReviewMode
      autocmd!
    augroup END
  endif
endfunction

" Check if we're still in review mode
function! s:CheckReviewMode()
  if !exists('g:review_mappings_enabled') || !g:review_mappings_enabled
    return
  endif

  " Get quickfix list title
  let l:qf_info = getqflist({'title': 1})

  " If title doesn't start with "Branch Review:", disable review mode
  if l:qf_info.title !~# '^Branch Review:'
    call s:DisableReviewMappings()
  endif
endfunction

" Jump to next/previous change - works in both diff mode and GitGutter
function! s:NextChange()
  if &diff
    " In diff mode, use native diff navigation
    normal! ]c
  else
    " Otherwise use GitGutter
    GitGutterNextHunk
  endif
endfunction

function! s:PrevChange()
  if &diff
    " In diff mode, use native diff navigation
    normal! [c
  else
    " Otherwise use GitGutter
    GitGutterPrevHunk
  endif
endfunction

" Navigate to next/previous quickfix item and open with diff
function! s:ReviewNext()
  try
    cnext
    " Close any existing diff windows
    only
    " Open diff for new file
    execute 'Ghdiffsplit ' . g:review_base_branch
  catch /^Vim\%((\a\+)\)\=:E/
    echohl WarningMsg | echo 'No more items' | echohl None
  endtry
endfunction

function! s:ReviewPrev()
  try
    cprev
    " Close any existing diff windows
    only
    " Open diff for new file
    execute 'Ghdiffsplit ' . g:review_base_branch
  catch /^Vim\%((\a\+)\)\=:E/
    echohl WarningMsg | echo 'No more items' | echohl None
  endtry
endfunction

" Get list of git branches for command completion
function! s:ListGitBranches(ArgLead, CmdLine, CursorPos)
  let l:branches = systemlist('git branch -a | sed "s/^[* ]*//" | sed "s#remotes/origin/##" | sort -u')
  return filter(l:branches, 'v:val =~ "^" . a:ArgLead')
endfunction

" Open current file normally (close diff view)
function! s:OpenThisFile()
  " If we're in a fugitive buffer, get the real file path
  if expand('%') =~# '^fugitive://'
    " Use Gedit to open the working tree version
    only
    Gedit
  else
    let l:filename = expand('%:p')
    only
    execute 'edit ' . fnameescape(l:filename)
  endif
endfunction

command! -bar -nargs=0 Branches               call <sid>GitSwitchBranch()
command! -range        GithubBrowse           <line1>,<line2>call <sid>GithubBrowse()
command! -nargs=? -complete=customlist,s:ListGitBranches ReviewBranch call <sid>ReviewBranch(<f-args>)
command! -bar -nargs=0 ReviewEnd              call <sid>DisableReviewMappings() | echom 'Review mode disabled'
command! -bar -nargs=0 OpenThisFile           call <sid>OpenThisFile()

" Global mappings for jumping to changes
nnoremap <silent> ]c :call <SID>NextChange()<CR>
nnoremap <silent> [c :call <SID>PrevChange()<CR>
