function! s:FzfGoModules()
  let l:gopath = substitute(system('go env GOPATH'), '\n', '', '')
  let l:mod_cache = l:gopath . '/pkg/mod'
  let l:cmd = 'find ' . shellescape(l:mod_cache) . ' -mindepth 1 -maxdepth 5 -type d'
  call fzf#run(fzf#wrap({
        \ 'source': l:cmd,
        \ 'sink': function('s:OpenDirInNewTab'),
        \ 'options': '--prompt="go_mod_cache> "'
        \ }))
endfunction

function! s:FzfPythonPackages()
  let l:cmd = 'find $(python -c "import site; print(site.getsitepackages()[0])") -mindepth 1 -maxdepth 1 -type d'
  call fzf#run(fzf#wrap({
        \ 'source': l:cmd,
        \ 'sink': function('s:OpenDirInNewTab'),
        \ 'options': '--prompt="site_packages> "'
        \ }))
endfunction

function! s:FzfNodeModules()
  let l:cmd = 'find node_modules -mindepth 1 -maxdepth 1 -type d'
  call fzf#run(fzf#wrap({
        \ 'source': l:cmd,
        \ 'sink': function('s:OpenDirInNewTab'),
        \ 'options': '--prompt="node_modules> "'
        \ }))
endfunction

function! s:OpenDirInNewTab(selected)
  execute 'tabnew'
  execute 'edit ' . fnameescape(a:selected)
  execute 'tcd '  . fnameescape(a:selected)
  execute '!ctags -R .'
endfunction

function! s:Highlight()
  redir @a | highlight | redir END | new | put a
endfunction

function! s:CleanUndoFiles()
  !find ~/.vim/tmp/undo -type f -mtime +100d \! -name '.gitignore' -delete
endfunction

function! s:ZoomToggle() abort
  if exists('t:zoomed') && t:zoomed
    execute t:zoom_winrestcmd
    let t:zoomed = 0
  else
    let t:zoom_winrestcmd = winrestcmd()
    resize
    vertical resize
    let t:zoomed = 1
  endif
endfunction

function! s:AddOpenBuffersToList()
  let qf_list = map(filter(range(1, bufnr('$')), 'buflisted(v:val)'), '{"bufnr": v:val}')
  call setqflist(qf_list)
  copen
endfunction

function! s:RemoveQfItem()
  let curqfidx = line('.') - 1
  let qfall = getqflist()
  call remove(qfall, curqfidx)
  call setqflist(qfall, 'r')
  execute curqfidx + 1 . "cfirst"
  :copen
endfunction

" http://ddrscott.github.io/blog/2016/vim-toggle-movement/
function! s:ToggleMovement(firstOp, thenOp)
  let pos = getpos('.')
  execute "normal! " . a:firstOp
  if pos == getpos('.')
    execute "normal! " . a:thenOp
  endif
endfunction

function! s:WinMoveOrSplit(key)
  let t:curwin = winnr()
  exec "wincmd ".a:key
  if (t:curwin == winnr())
    if (match(a:key,'[jk]'))
      wincmd v
    else
      wincmd s
    endif
    exec "wincmd ".a:key
  endif
endfunction

function! s:SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunction

function! s:ViewGitHistory() abort
  Git difftool --name-only ! !^@
  call s:diff_current_quickfix_entry()
  " Bind <CR> for current quickfix window to properly set up diff split layout after selecting an item
  " There's probably a better way to map this without changing the window
  copen
  nnoremap <buffer> <CR> <CR><BAR>:call <sid>diff_current_quickfix_entry()<CR>
  wincmd p
endfunction

function! s:diff_current_quickfix_entry() abort
  " Cleanup windows
  for window in getwininfo()
    if window.winnr !=? winnr() && bufname(window.bufnr) =~? '^fugitive:'
      exe 'bdelete' window.bufnr
    endif
  endfor
  cc
  call s:add_mappings()
  let qf = getqflist({'context': 0, 'idx': 0})
  if get(qf, 'idx') && type(get(qf, 'context')) == type({}) && type(get(qf.context, 'items')) == type([])
    let diff = get(qf.context.items[qf.idx - 1], 'diff', [])
    echom string(reverse(range(len(diff))))
    for i in reverse(range(len(diff)))
      exe (i ? 'leftabove' : 'rightbelow') 'vert diffsplit' fnameescape(diff[i].filename)
      call s:add_mappings()
    endfor
  endif
endfunction

function! s:add_mappings() abort
  nnoremap <buffer>]q :cnext <BAR> :call <sid>diff_current_quickfix_entry()<CR>
  nnoremap <buffer>[q :cprevious <BAR> :call <sid>diff_current_quickfix_entry()<CR>
  " Reset quickfix height. Sometimes it messes up after selecting another item
  11copen
  wincmd p
endfunction

function! s:OpenGodotDocs()
  let l:cmd = 'find ~/opt/godot-docs -type f -name "*.html"'
  call fzf#run(fzf#wrap({
        \ 'source': l:cmd,
        \ 'sink': function('s:OsOpenSelection'),
        \ 'options': '--prompt="Godot Docs> "'
        \ }))
endfunction

function! s:OsOpenSelection(selected)
  let l:cmd = 'open ' . fnameescape(a:selected)
  call system(l:cmd)
endfunction

command! -bar -nargs=0 DiffHistory       call <sid>ViewGitHistory()
command! -bar -nargs=1 GrepMotion        call <sid>GrepMotion(<q-args>)
command! -bar -nargs=1 OpenDirInNewTab   call <sid>OpenDirInNewTab(<q-args>)
command! -bar -nargs=1 OsOpenSelection   call <sid>OsOpenSelection(<q-args>)
command! -bar -nargs=1 WinMoveOrSplit    call <sid>WinMoveOrSplit(<q-args>)
command! -bar -nargs=+ ToggleMovement    call <sid>ToggleMovement(<f-args>)
command! -bar -nargs=0 RemoveQfItem      call <sid>RemoveQfItem()
command! -bar -nargs=0 Highlight         call <sid>Highlight()
command! -bar -nargs=0 CleanUndoFiles    call <sid>CleanUndoFiles()
command! -bar -nargs=0 QfBuffers         call <sid>AddOpenBuffersToList()
command! -bar -nargs=0 GodotDocs         call <sid>OpenGodotDocs()
command! -bar -nargs=0 SynStack          call <sid>SynStack()

nnoremap <Plug>ZoomToggle                :call <sid>ZoomToggle()<cr>
nnoremap <Plug>FzfNodeModules            :call <sid>FzfNodeModules()<cr>
nnoremap <Plug>FzfPythonPackages         :call <sid>FzfPythonPackages()<cr>
nnoremap <Plug>FzfGoModules              :call <sid>FzfGoModules()<cr>

function! s:InsertRelativeFilePath()
  let l:cwd = getcwd()
  let l:file = expand('%:p')
  if l:file == ''
    echo "No file associated with this buffer"
    return
  endif
  let l:relative_path = substitute(l:file, l:cwd . '/', '', '')
  execute "normal! i" . "[" . l:relative_path . "] \<ESC>"
endfunction

function! s:CopyRelativeFilePath()
  let l:root = getcwd()
  let l:file = expand('%:p')
  if l:file == ''
    echo "No file associated with this buffer"
    return
  endif
  let l:relative_path = substitute(l:file, l:root . '/', '', '')
  let @+ = l:relative_path
  echo "Copied: " . l:relative_path
endfunction

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

function! s:GitDiffToQuickfix(range)
  " Check if we're in a git repo
  call system('git rev-parse --git-dir 2>/dev/null')
  if v:shell_error != 0
    echohl ErrorMsg
    echom "Not in a git repository"
    echohl None
    return
  endif

  let l:root = getcwd()

  " If no range provided, default to merge-base with main/master
  if empty(a:range)
    " Try to find main or master branch
    let l:base = 'main'
    call system('git rev-parse --verify ' . shellescape(l:base) . ' 2>/dev/null')
    if v:shell_error != 0
      let l:base = 'master'
      call system('git rev-parse --verify ' . shellescape(l:base) . ' 2>/dev/null')
      if v:shell_error != 0
        echohl ErrorMsg
        echom "No main or master branch found. Provide a range like: origin/main...HEAD"
        echohl None
        return
      endif
    endif
    let l:range = l:base . '...HEAD'
  else
    let l:range = a:range
  endif

  echom "Getting diff for: " . l:range

  " Get the diff
  let l:diff = systemlist('git diff ' . l:range . ' -U0 2>&1')

  if v:shell_error != 0
    echohl ErrorMsg
    echom "Failed to get diff: " . join(l:diff, ' ')
    echohl None
    return
  endif

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

  if empty(l:qflist)
    echom "No changes found for: " . l:range
    return
  endif

  call setqflist(l:qflist)
  echom "Loaded " . len(l:qflist) . " changes (" . l:range . ") into quickfix"
  copen
endfunction

" Must be global - called by tabline setting
function! MyTabLine()
  let s = ''
  for i in range(tabpagenr('$'))
    " Select the highlighting
    if i + 1 == tabpagenr()
      let s .= '%#TabLineSel#'
    else
      let s .= '%#TabLine#'
    endif
    " Set the tab page number (for mouse clicks)
    let s .= '%' . (i + 1) . 'T'
    " Get the buffer list and window number for the tab page
    let buflist = tabpagebuflist(i + 1)
    let winnr = tabpagewinnr(i + 1)
    " Determine the name to use: custom or file name
    if has_key(g:tabline_custom_names, i + 1)
      let name = g:tabline_custom_names[i + 1]
    else
      let name = fnamemodify(bufname(buflist[winnr - 1]), ':t')
      if empty(name)
        let name = ' ? '
      endif
    endif
    " Append the name to the tab line
    let s .= ' ' . name . ' '
  endfor
  " Fill the rest of the tabline
  let s .= '%#TabLineFill#%T'
  return s
endfunction

function! s:RenameCurrentTab(new_name)
  " Store the new name in the dictionary for the current tab
  let g:tabline_custom_names[tabpagenr()] = a:new_name
endfunction

function! s:CleanupHelpBuffers()
  for buf in getbufinfo({'buflisted': 1})
    if getbufvar(buf.bufnr, '&filetype') ==# 'help'
      execute 'bwipeout' buf.bufnr
    endif
  endfor
  echomsg "Help buffers cleaned up"
endfunction

function! s:ReuseOrCreateTerminal(args) abort
  " Expand % now before switching windows
  let expanded_args = substitute(a:args, '%', expand('%'), 'g')

  " Find first visible terminal window
  let term_winnr = -1
  for winnr in range(1, winnr('$'))
    if getwinvar(winnr, '&buftype') ==# 'terminal'
      let term_winnr = winnr
      break
    endif
  endfor

  if term_winnr != -1
    " Terminal exists - reuse it
    execute term_winnr . 'wincmd w'
    execute 'term ++curwin ' . expanded_args
  else
    " No terminal - create new
    execute 'term ' . expanded_args
  endif
endfunction

function! s:GitCheckoutBranch(branch)
  " Remove leading/trailing whitespace and any * marker
  let l:branch = substitute(trim(a:branch), '^\*\s*', '', '')
  let l:branch = trim(l:branch)

  if empty(l:branch)
    return
  endif

  " Check if it's a remote branch (origin/...)
  if l:branch =~# '^origin/'
    " Extract branch name without origin/
    let l:local_branch = substitute(l:branch, '^origin/', '', '')

    " Check if local branch already exists
    call system('git rev-parse --verify ' . shellescape(l:local_branch) . ' 2>/dev/null')

    if v:shell_error == 0
      " Local branch exists, just check it out
      let l:cmd = 'git checkout ' . shellescape(l:local_branch)
    else
      " Create new local branch tracking remote
      let l:cmd = 'git checkout -b ' . shellescape(l:local_branch) . ' --track ' . shellescape(l:branch)
    endif
  else
    " Local branch, just check it out
    let l:cmd = 'git checkout ' . shellescape(l:branch)
  endif

  " Execute checkout
  let l:output = system(l:cmd . ' 2>&1')

  if v:shell_error != 0
    " Check if it's due to uncommitted changes
    if l:output =~# 'Your local changes.*would be overwritten'
      echohl WarningMsg
      echom "Git: Uncommitted changes blocking checkout"
      echohl None
      let l:choice = confirm("Switch anyway?", "&Force\n&Stash\n&Cancel", 3)
      if l:choice == 1
        " Force checkout
        let l:force_output = system(l:cmd . ' --force 2>&1')
        if v:shell_error == 0
          echom "Switched to: " . l:branch . " (forced)"
        else
          echohl ErrorMsg
          echom "Failed: " . trim(l:force_output)
          echohl None
        endif
      elseif l:choice == 2
        " Stash and checkout
        call system('git stash push -m "Auto-stash before switching to ' . l:branch . '" 2>&1')
        let l:stash_output = system(l:cmd . ' 2>&1')
        if v:shell_error == 0
          echom "Switched to: " . l:branch . " (changes stashed)"
        else
          echohl ErrorMsg
          echom "Failed: " . trim(l:stash_output)
          echohl None
        endif
      endif
    else
      echohl ErrorMsg
      echom "Git checkout failed: " . substitute(trim(l:output), '\n', ' ', 'g')
      echohl None
    endif
  else
    " Reload current buffer if it has a filename
    if expand('%') != ''
      silent! edit
    endif
    echom "Switched to: " . l:branch
    " Trigger any git-related plugins to update
    silent! doautocmd User FugitiveChanged
  endif
endfunction

function! s:FzfGitBranches()
  " Get all branches (local and remote), remove HEAD entries
  let l:cmd = 'git branch -a | grep -v HEAD | sed "s/^[* ]*//" | sed "s#remotes/##"'

  call fzf#run(fzf#wrap({
        \ 'source': l:cmd,
        \ 'sink': function('s:GitCheckoutBranch'),
        \ 'options': ['--prompt=Branch> ', '--preview', 'git log --oneline --graph --date=short --pretty="format:%C(auto)%cd %h%d %s" $(echo {} | sed "s#origin/##") --color=always | head -50']
        \ }))
endfunction

function! s:CommentOnPRLine()
  " Get current line number
  let l:line_num = line('.')

  " Get file path relative to git root
  let l:git_root = systemlist('git rev-parse --show-toplevel')[0]
  let l:file_path = expand('%:p')
  let l:relative_path = substitute(l:file_path, l:git_root . '/', '', '')

  " Get current branch
  let l:branch = systemlist('git branch --show-current')[0]

  " Get PR number for current branch
  let l:pr_result = systemlist('gh pr view --json number -q .number')
  if v:shell_error != 0 || empty(l:pr_result)
    echohl ErrorMsg
    echo "No PR found for branch: " . l:branch
    echohl None
    return
  endif
  let l:pr_number = l:pr_result[0]

  " Get latest commit SHA
  let l:commit_sha = systemlist('git rev-parse HEAD')[0]

  " Get repo info (owner/repo)
  let l:repo_info = systemlist('gh repo view --json nameWithOwner -q .nameWithOwner')[0]

  " Prompt for comment
  call inputsave()
  let l:comment = input('Comment: ')
  call inputrestore()

  if empty(l:comment)
    echo "Cancelled"
    return
  endif

  " Post comment using gh api
  let l:gh_cmd = printf(
    \ "gh api repos/%s/pulls/%s/comments -f body='%s' -f commit_id='%s' -f path='%s' -F line=%d -f side='RIGHT'",
    \ l:repo_info,
    \ l:pr_number,
    \ escape(l:comment, "'"),
    \ l:commit_sha,
    \ l:relative_path,
    \ l:line_num
    \ )

  let l:result = system(l:gh_cmd)

  if v:shell_error != 0
    echohl ErrorMsg
    echo "Failed to post comment: " . l:result
    echohl None
  else
    echo "Comment posted on " . l:relative_path . ":" . l:line_num
  endif
endfunction

command! -nargs=* -complete=shellcmd Term call s:ReuseOrCreateTerminal(<q-args>)
command! -bar -nargs=0 InsertRelativeFilePath  call <sid>InsertRelativeFilePath()
command! -bar -nargs=0 CopyRelativeFilePath    call <sid>CopyRelativeFilePath()
command! -bar -nargs=0 GitUnstagedToQuickfix   call <sid>GitUnstagedToQuickfix()
command! -bar -nargs=? GitDiffToQuickfix       call <sid>GitDiffToQuickfix(<q-args>)
command! -bar -nargs=1 RenameTab               call <sid>RenameCurrentTab(<q-args>)
command! -bar -nargs=0 CleanupHelp             call <sid>CleanupHelpBuffers()
command! -bar -nargs=0 CommentOnPRLine         call <sid>CommentOnPRLine()
command! -bar -nargs=0 Branches                call <sid>FzfGitBranches()
