function! s:SynStack() abort
  if !exists("*synstack")
    echom "No synstack function available!"
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunction

command! -bar -nargs=0 SynStack call <sid>SynStack()
