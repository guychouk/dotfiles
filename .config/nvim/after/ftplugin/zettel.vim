set textwidth=80
set suffixesadd+=.zettel
set path+=,/Users/apedev/Projects/personal/quwiki

nnoremap <silent> gt :exe "edit " . expand("**/" . expand("<cword>") . "**")<CR>
nnoremap <silent> <Leader>id :execute "normal! i " . strftime("%Y%m%d%H%M%S")<CR>
nnoremap <silent> <Leader>lz :LZ<CR>

function! LinkZettel(val)
		let zid = split(a:val, '-')[0]
		execute "normal! i [" . zid . "]\<Esc>"
endfunction

command! LZ :call fzf#run(fzf#wrap({'sink': funcref('LinkZettel')}))
