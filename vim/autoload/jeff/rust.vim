" Resolve a crate name to its source directory via `cargo metadata`, which
" knows the registry, git and path locations of every dependency (and of the
" workspace's own crates). Only the first `::` segment is a crate; crate::,
" self:: and super:: return empty so gf falls through to the builtin, which
" uses the stock ftplugin's includeexpr. --filter-platform keeps --offline
" from failing on target-specific deps that were never downloaded.
function! jeff#rust#resolve(spec) abort
  let l:name = substitute(split(a:spec, '::')[0], '-', '_', 'g')
  if l:name =~# '^\(crate\|self\|super\)$'
    return ''
  endif
  let l:host = matchstr(system('rustc -vV'), 'host: \zs[^[:space:]]\+')
  let l:cwd = getcwd()
  try
    execute 'lcd' fnameescape(expand('%:p:h'))
    let l:out = system(join(map(['cargo', 'metadata', '--format-version', '1',
          \ '--offline', '--filter-platform', l:host], {_, v -> shellescape(v)})) . ' 2>/dev/null')
  finally
    execute 'lcd' fnameescape(l:cwd)
  endtry
  if v:shell_error != 0
    return ''
  endif
  for l:pkg in json_decode(l:out).packages
    if substitute(l:pkg.name, '-', '_', 'g') ==# l:name
      return fnamemodify(l:pkg.manifest_path, ':h')
    endif
  endfor
  return ''
endfunction
