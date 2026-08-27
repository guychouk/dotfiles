setlocal suffixesadd=.go

command! -buffer -nargs=* GoKeywordPrg call godoc#open()

nnoremap <buffer> <silent> gf :call jeff#open(function('jeff#go#resolve'))<CR>
nnoremap <buffer> <localleader>b :compiler go<Bar>Compile<CR>
nnoremap <buffer> <localleader>r :Term go run %<CR>
nnoremap <buffer> <localleader>t :compiler gotest<Bar>Compile<CR>
nnoremap <buffer> <localleader>l :compiler golangci-lint<Bar>Compile<CR>
