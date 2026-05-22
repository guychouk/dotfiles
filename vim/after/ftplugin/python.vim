setlocal foldmethod=indent
setlocal suffixesadd=.py
setlocal include=^\s*\(import\|from\)
setlocal includeexpr=substitute(v:fname,'\\.','\/','g')

nnoremap <buffer> <localleader>r :Term python3 %<CR>
nnoremap <buffer> <localleader>R :Compile python3<CR>
nnoremap <buffer> <localleader>l :Compile ruff<CR>
