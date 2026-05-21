if !exists('*s:DirvishEnter')
  function s:DirvishEnter() abort
    let l:path = trim(getline('.'))
    if l:path =~? '\.\(png\|jpg\|jpeg\|gif\|bmp\|webp\|tiff\|ico\)$'
      call system('kitty @ launch --type=overlay fish -c ' . shellescape('kitty icat ' . shellescape(l:path) . '; read -n1'))
    else
      call dirvish#open('edit', 0)
    endif
  endfunction
endif

nnoremap <buffer> <nowait> <silent> <CR> <cmd>call <SID>DirvishEnter()<CR>
nnoremap <buffer> <nowait> <silent> t    <cmd>call dirvish#open('tabedit', 0)<CR>
xnoremap <buffer> <nowait> <silent> t    <cmd>call dirvish#open('tabedit', 0)<CR>
