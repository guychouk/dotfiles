" Minimal package manager
"
"   let g:packages = [
"     \ 'tpope/vim-fugitive',
"     \ 'junegunn/fzf.vim',
"   \ ]
"
"   :PackInstall   - clone missing packages
"   :PackUpdate    - git pull all packages
"   :PackClean     - remove unlisted packages

let s:pack_dir = expand('~/.vim/pack/bundle/start')

function! s:repo_to_name(repo)
  return substitute(a:repo, '^.*/', '', '')
endfunction

function! s:echo(msg, hl)
  execute 'echohl' a:hl
  echo a:msg
  echohl None
endfunction

function! pack#install()
  if !isdirectory(s:pack_dir)
    call mkdir(s:pack_dir, 'p')
  endif

  for repo in get(g:, 'packages', [])
    let name = s:repo_to_name(repo)
    let path = s:pack_dir . '/' . name

    if !isdirectory(path)
      call s:echo('Installing ' . name . '...', 'MoreMsg')
      let url = repo =~# '^https\?://' ? repo : 'https://github.com/' . repo
      let cmd = 'git clone --depth=1 ' . shellescape(url) . ' ' . shellescape(path)
      let out = system(cmd)
      if v:shell_error
        call s:echo('Failed: ' . name, 'ErrorMsg')
        echo out
      else
        call s:echo('Installed: ' . name, 'MoreMsg')
      endif
    else
      call s:echo('Already installed: ' . name, 'Comment')
    endif
  endfor

  call s:echo('Done!', 'MoreMsg')
endfunction

function! pack#update()
  let packages = get(g:, 'packages', [])
  if empty(packages)
    call s:echo('No packages defined in g:packages', 'WarningMsg')
    return
  endif

  for repo in packages
    let name = s:repo_to_name(repo)
    let path = s:pack_dir . '/' . name

    if isdirectory(path)
      call s:echo('Updating ' . name . '...', 'MoreMsg')
      let cmd = 'git -C ' . shellescape(path) . ' pull --rebase'
      let out = system(cmd)
      if v:shell_error
        call s:echo('Failed: ' . name, 'ErrorMsg')
      else
        echo out
      endif
    endif
  endfor

  call s:echo('Done!', 'MoreMsg')
endfunction

function! pack#clean()
  let packages = get(g:, 'packages', [])
  let installed = glob(s:pack_dir . '/*', 0, 1)
  let keep = map(copy(packages), 's:pack_dir . "/" . s:repo_to_name(v:val)')

  for path in installed
    if index(keep, path) == -1
      let name = fnamemodify(path, ':t')
      call s:echo('Removing ' . name . '...', 'WarningMsg')
      call delete(path, 'rf')
    endif
  endfor

  call s:echo('Done!', 'MoreMsg')
endfunction

command! PackInstall call pack#install()
command! PackUpdate  call pack#update()
command! PackClean   call pack#clean()
