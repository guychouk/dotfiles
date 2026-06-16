setlocal tabstop=2
setlocal shiftwidth=2
setlocal suffixesadd=.ts,.tsx,.js,.jsx,.json
setlocal include=from\s*['\"]\|require(\s*['\"]
setlocal includeexpr=v:fname=~#'^[./]'?v:fname:'node_modules/'.v:fname.'/index'
setlocal isfname+=@-@
nnoremap <buffer> <silent> gf :call jsgf#open()<CR>
nnoremap <buffer> <localleader>b :Compile tsc<CR>
nnoremap <buffer> <localleader>r :Term bun %<CR>
nnoremap <buffer> <localleader>R :Compile bun<CR>
nnoremap <buffer> <localleader>l :Compile eslint<CR>
