function! utils#GrepMotion(type) abort
  " Save the current unnamed register
  let l:save_reg = @"
  " Determine the range of the visual selection
  if a:type ==# 'v' || a:type ==# 'char'
    normal! `<v`>y
  elseif a:type ==# 'V' || a:type ==# 'line'
    normal! `<V`>y
  elseif a:type ==# "\<C-v>" || a:type ==# 'block'
    normal! `<\<C-v>`>y
  endif
  " Get the yanked text
  let l:text = escape(@", '"\')
  " Restore the unnamed register
  let @" = l:save_reg
  " Execute the grep command with the selected text
  execute 'grep "' . l:text . '"'
endfunction
