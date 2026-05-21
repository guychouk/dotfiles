setlocal path+=**
setlocal suffixesadd=.js,.jsx,.ts,.tsx,.json
setlocal include=from\s*['\"]\|require(\s*['\"]
setlocal includeexpr=v:fname=~#'^[./]'?v:fname:'node_modules/'.v:fname.'/index'
setlocal tabstop=4
setlocal shiftwidth=4
nnoremap <buffer> <localleader>b :Compile<CR>
nnoremap <buffer> <localleader>r :Term node %<CR>
nnoremap <buffer> <localleader>R :Compile node<CR>
nnoremap <buffer> <localleader>l :Compile eslint<CR>
