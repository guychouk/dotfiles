set textwidth=80
set suffixesadd+=.zettel
set path+=,/Users/apedev/Projects/personal/zettzim

function! LinkZettel(val)
		let zid = split(a:val, '-')[0]
		execute "normal! i [" . zid . "]\<Esc>"
endfunction

command! LinkZettel :call fzf#run(fzf#wrap({'sink': funcref('LinkZettel')}))

nnoremap <silent> <Leader>q :qa<CR>
nnoremap <silent> <Leader>l :LinkZettel<CR>
nnoremap <silent> gf :exe "edit " . expand("**/" . expand("<cword>") . "**")<CR>
