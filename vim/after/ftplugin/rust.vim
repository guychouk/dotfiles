setlocal suffixesadd=.rs

nnoremap <buffer> <silent> gf :call jeff#open(function('jeff#rust#resolve'))<CR>

nnoremap <buffer> <localleader>b :compiler cargo<Bar>Compile cargo build --color=never<CR>
nnoremap <buffer> <localleader>r :Term cargo run<CR>
nnoremap <buffer> <localleader>t :compiler cargotest<Bar>Compile<CR>
nnoremap <buffer> <localleader>l :compiler cargo<Bar>Compile cargo clippy --color=never<CR>
