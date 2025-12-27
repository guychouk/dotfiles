function! s:ExploreGoPackages()
  let l:gopath = substitute(system('go env GOPATH'), '\n', '', '')
  let l:mod_cache = l:gopath . '/pkg/mod'
  let l:cmd = 'find ' . shellescape(l:mod_cache) . ' -mindepth 1 -maxdepth 5 -type d'
  call fzf#run(fzf#wrap({
        \ 'source': l:cmd,
        \ 'sink': function('s:OpenDirInNewTab'),
        \ 'options': '--prompt="go_packages> "'
        \ }))
endfunction

function! s:ExplorePythonPackages()
  let l:cmd = 'find $(python -c "import site; print(site.getsitepackages()[0])") -mindepth 1 -maxdepth 1 -type d'
  call fzf#run(fzf#wrap({
        \ 'source': l:cmd,
        \ 'sink': function('s:OpenDirInNewTab'),
        \ 'options': '--prompt="python_packages> "'
        \ }))
endfunction

function! s:ExploreNodeModules()
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
  execute '!ctags .'
endfunction

function! s:ExploreGodotDocs()
  let l:cmd = 'find ~/opt/godot-docs -type f -name "*.html"'
  call fzf#run(fzf#wrap({
        \ 'source': l:cmd,
        \ 'sink': function('s:OsOpenSelection'),
        \ 'options': '--prompt="godot_docs> "'
        \ }))
endfunction

function! s:OsOpenSelection(selected)
  let l:cmd = 'open ' . fnameescape(a:selected)
  call system(l:cmd)
endfunction

command! -bar -nargs=1 OpenDirInNewTab   call <sid>OpenDirInNewTab(<q-args>)
command! -bar -nargs=1 OsOpenSelection   call <sid>OsOpenSelection(<q-args>)
command! -bar -nargs=0 GodotDocs         call <sid>ExploreGodotDocs()

nnoremap <Plug>ExploreNodeModules        :call <sid>ExploreNodeModules()<cr>
nnoremap <Plug>ExplorePythonPackages     :call <sid>ExplorePythonPackages()<cr>
nnoremap <Plug>ExploreGoPackages         :call <sid>ExploreGoPackages()<cr>
