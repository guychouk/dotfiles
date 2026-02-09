" Plugin for searching packages
"
" Supports go, node.js, python and vim.

let s:package_base_dir = ''

function! s:SearchPackages(type) abort
  let l:sources = {
        \ 'go': {
        \   'dir': substitute(system('go env GOPATH'), '\n', '', '') . '/pkg/mod',
        \   'depth': 5
        \ },
        \ 'python': {
        \   'dir': substitute(system('python -c "import site; print(site.getsitepackages()[0])"'), '\n', '', ''),
        \   'depth': 1
        \ },
        \ 'node': {
        \   'dir': getcwd() . '/node_modules',
        \   'depth': 1
        \ },
        \ 'vim': {
        \   'dir': expand('~/.vim/pack/bundle/start'),
        \   'depth': 1
        \ }
        \ }
  let l:config = l:sources[a:type]
  let s:package_base_dir = l:config.dir
  let l:cmd = 'find . -mindepth 1 -maxdepth ' . l:config.depth . ' -type d'
  call fzf#run(
        \ fzf#wrap({
        \   'source': l:cmd,
        \   'dir': l:config.dir,
        \   'sink': function('s:OpenPackageDir'),
        \   'options': '--prompt="' . a:type . '_packages> "'
        \ }, 0))
endfunction

function! s:OpenPackageDir(selected)
  let l:rel_path = substitute(a:selected, '^\.\/', '', '')
  let l:full_path = s:package_base_dir . '/' . l:rel_path
  " Create a safe temp filename based on the package path
  let l:safe_name = substitute(l:full_path, '[^a-zA-Z0-9]', '_', 'g')
  let l:tags_dir = expand('~/.cache/vim/tags')
  let l:tags_file = l:tags_dir . '/tags-' . l:safe_name
  " Ensure tags directory exists
  if !isdirectory(l:tags_dir)
    call mkdir(l:tags_dir, 'p')
  endif
  execute 'tabnew'
  execute 'tcd '  . fnameescape(l:full_path)
  execute 'edit ' . fnameescape(l:full_path)
  execute 'Dispatch ctags .'
endfunction

function! s:SearchGodotDocs()
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

command! -bar -nargs=0 GodotDocs            call <sid>SearchGodotDocs()
command! -bar -nargs=0 SearchGoPackages     call <sid>SearchPackages('go')
command! -bar -nargs=0 SearchVimPackages    call <sid>SearchPackages('vim')
command! -bar -nargs=0 SearchNodeModules    call <sid>SearchPackages('node')
command! -bar -nargs=0 SearchPythonPackages call <sid>SearchPackages('python')
command! -bar -nargs=1 OsOpen               call <sid>OsOpenSelection(<q-args>)
command! -bar -nargs=1 OpenDirInNewTab      call <sid>OpenPackageDir(<q-args>)
