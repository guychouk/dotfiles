setlocal tabstop=4
setlocal shiftwidth=4
setlocal foldmethod=syntax
let c_no_curly_error = 1

nnoremap <buffer> <localleader>b :compiler nob<Bar>Compile<CR>
