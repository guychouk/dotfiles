" Fuzzy-find across g:surf_dirs (depends on fd, fzf & junegunn/fzf).
"
" :Surf opens a file in place
" :SurfDir zaps into a directory as a new tcd'd tab.

if !executable('fzf') || !executable('fd')
  finish
endif

let g:surf_dirs = get(g:, 'surf_dirs', ['~/src', '~/Documents', '~/dotfiles', '~/Downloads'])

function! s:surf_open(line) abort
  execute 'edit' fnameescape(a:line)
endfunction

function! s:surf_zap(line) abort
  let l:full = fnamemodify(a:line, ':p')
  execute 'tabedit' fnameescape(l:full)
  execute 'tcd' fnameescape(l:full)
endfunction

function! s:surf(fdtype, Sink) abort
  let l:roots = join(map(copy(g:surf_dirs), {_, v -> shellescape(expand(v))}), ' ')
  let l:find = 'fd --type ' . a:fdtype . ' --hidden --exclude .git --exclude node_modules . ' . l:roots
  let l:source = a:fdtype ==# 'd' ? 'printf ''%s/\n'' ' . l:roots . '; ' . l:find : l:find
  call fzf#run(fzf#wrap({
        \ 'source': l:source,
        \ 'sink': a:Sink,
        \ 'down': '10',
        \ 'options': ['--margin=2%,0%', '--info=hidden', '--no-scrollbar', '--prompt=🔎 '],
        \ }))
endfunction

command! -bar Surf    call <sid>surf('f', function('<sid>surf_open'))
command! -bar SurfDir call <sid>surf('d', function('<sid>surf_zap'))
