setlocal tabstop=4
setlocal shiftwidth=4
setlocal suffixesadd=.js,.jsx,.ts,.tsx,.json
setlocal include=from\s*['\"]\|require(\s*['\"]
setlocal includeexpr=v:fname=~#'^[./]'?v:fname:'node_modules/'.v:fname.'/index'
setlocal isfname+=@-@
nnoremap <buffer> <silent> gf :call jsgf#open()<CR>
nnoremap <buffer> <localleader>b :Compile<CR>
nnoremap <buffer> <localleader>r :Term node %<CR>
nnoremap <buffer> <localleader>R :compiler node<Bar>Compile<CR>
nnoremap <buffer> <localleader>l :compiler eslint<Bar>Compile<CR>
