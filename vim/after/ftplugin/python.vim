setlocal foldmethod=indent
setlocal suffixesadd=.py
setlocal include=^\s*\(import\|from\)
setlocal includeexpr=substitute(v:fname,'\\.','\/','g')

nnoremap <buffer> <silent> gf :call jeff#open(function('jeff#python#resolve'))<CR>
nnoremap <buffer> <localleader>r :Term python3 %<CR>
nnoremap <buffer> <localleader>R :compiler python3<Bar>Compile<CR>
nnoremap <buffer> <localleader>l :compiler ruff<Bar>Compile<CR>
