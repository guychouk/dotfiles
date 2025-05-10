function! s:FzfGoModules()
  let l:gopath = system('go env GOPATH')
  let l:gopath = substitute(l:gopath, '\n', '', '')
  let l:mod_cache = l:gopath . '/pkg/mod'
  let l:cmd = 'find ' . shellescape(l:mod_cache) . ' -mindepth 1 -maxdepth 3 -type d'
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
