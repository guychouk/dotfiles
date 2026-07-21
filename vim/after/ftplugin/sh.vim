setlocal suffixesadd=.sh
setlocal include=^\s*\.\s\+\|^\s*source\s\+
nnoremap <buffer> <localleader>l :compiler shellcheck<Bar>Compile<CR>
