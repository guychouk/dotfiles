function! s:SynStack() abort
  if !exists("*synstack")
    echom "No synstack function available!"
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunction

" Taken from https://ddrscott.github.io/blog/2016/vim-toggle-movement
function! s:ToggleMovement(firstOp, thenOp) abort
  let pos = getpos('.')
  execute "normal! " . a:firstOp
  if pos == getpos('.')
    execute "normal! " . a:thenOp
  endif
endfunction

command! -bar -nargs=0 SynStack        call <sid>SynStack()
command! -bar -nargs=+ ToggleMovement  call <sid>ToggleMovement(<f-args>)
