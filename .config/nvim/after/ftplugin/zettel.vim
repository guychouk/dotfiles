set textwidth=80
set suffixesadd+=.zettel
set path+=,/Users/apedev/Projects/personal/zettzim

nnoremap <silent> <Leader>q :qa<CR>
nnoremap <silent> <Leader>l :LinkZettel<CR>
nnoremap <silent> <Leader>zk :ZettelkastenTemplate<CR>
nnoremap <silent> <Leader>id :execute "normal! i " . strftime("%Y%m%d%H%M%S")<CR>
nnoremap <silent> gf :exe "edit " . expand("**/" . expand("<cword>") . "**")<CR>

function! LinkZettel(val)
		let zid = split(a:val, '-')[0]
		execute "normal! i [" . zid . "]\<Esc>"
endfunction

function! ZettelkastenTemplate()
	0r ~/.config/nvim/skeleton.help
	execute '%s/%id%/' . strftime("%Y%m%d%H%M%S")
endfunction

command! ZettelkastenTemplate :call ZettelkastenTemplate()
command! LinkZettel :call fzf#run(fzf#wrap({'sink': funcref('LinkZettel')}))
