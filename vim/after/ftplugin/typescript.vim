setlocal tabstop=2
setlocal shiftwidth=2
setlocal isfname+=@-@
setlocal suffixesadd=.ts,.tsx,.js,.jsx,.json
setlocal include=from\s*['\"]\|require(\s*['\"]
setlocal includeexpr=v:fname=~#'^[./]'?v:fname:'node_modules/'.v:fname.'/index'

nnoremap <buffer> <silent> gf :call jeff#open(function('jeff#js#resolve'))<CR>
nnoremap <buffer> <localleader>b :compiler tsc<Bar>Compile<CR>
nnoremap <buffer> <localleader>r :Term bun %<CR>
nnoremap <buffer> <localleader>R :compiler bun<Bar>Compile<CR>
nnoremap <buffer> <localleader>l :compiler eslint<Bar>Compile<CR>
