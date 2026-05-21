setlocal path+=**
setlocal suffixesadd=.go
nnoremap <buffer> <localleader>b :Compile go<CR>
nnoremap <buffer> <localleader>r :Term go run %<CR>
nnoremap <buffer> <localleader>t :Compile gotest<CR>
nnoremap <buffer> <localleader>l :Compile golangci-lint<CR>
