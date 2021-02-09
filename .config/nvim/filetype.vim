function! LinkZettel(val)
		let zid = split(a:val, '-')[0]
		execute "normal! i [" . zid . "]\<Esc>"
endfunction

function! SetupZettelkasten()
	set textwidth=80
	nnoremap <silent> <Leader>q :qa<CR>
	nnoremap <silent> <Leader>l :LinkZettel<CR>
	nnoremap <silent> gf :exe "edit " . expand("**/" . expand("<cword>") . "**")<CR>
	command! LinkZettel :call fzf#run(fzf#wrap({'sink': funcref('LinkZettel')}))
endfunction

command! SetupZettelkasten :call SetupZettelkasten()

if exists("did_load_filetypes")
	finish
endif
augroup filetypedetect
	au! BufRead tmux.config setfiletype tmux
	au! BufRead,BufNewFile **/zetz/*.md SetupZettelkasten
	au! FileType qf map <silent> <buffer> dd :RemoveQFItem<cr>
	au! FileType gitcommit,gitrebase,gitconfig set bufhidden=delete
augroup END
