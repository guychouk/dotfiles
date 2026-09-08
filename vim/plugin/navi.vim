" Fuzzy-find across g:navi_dirs (depends on fd, fzf & junegunn/fzf).
"
" :Navi opens a file in place
" :NaviDir zaps into a directory as a new tcd'd tab.

if !executable('fzf') || !executable('fd')
  finish
endif

let g:navi_dirs = get(g:, 'navi_dirs', ['~/src', '~/Documents', '~/dotfiles', '~/Downloads'])

function! s:navi_open(line) abort
  execute 'edit' fnameescape(a:line)
endfunction

function! s:navi_zap(line) abort
  let l:full = fnamemodify(a:line, ':p')
  execute 'tabedit' fnameescape(l:full)
  execute 'tcd' fnameescape(l:full)
endfunction

function! s:navi(fdtype, Sink) abort
  let l:roots = join(map(copy(g:navi_dirs), {_, v -> shellescape(expand(v))}), ' ')
  let l:find = 'fd --type ' . a:fdtype . ' --hidden --exclude .git --exclude node_modules . ' . l:roots
  let l:source = a:fdtype ==# 'd' ? 'printf ''%s/\n'' ' . l:roots . '; ' . l:find : l:find
  call fzf#run(fzf#wrap({
        \ 'source': l:source,
        \ 'sink': a:Sink,
        \ 'down': '10',
        \ 'options': ['--margin=2%,0%', '--info=hidden', '--no-scrollbar', '--prompt=λ ', '--color=prompt:#e58a2b'],
        \ }))
endfunction

command! -bar Navi    call <sid>navi('f', function('<sid>navi_open'))
command! -bar NaviDir call <sid>navi('d', function('<sid>navi_zap'))
