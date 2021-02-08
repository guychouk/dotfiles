if exists("did_load_filetypes")
	finish
endif
augroup filetypedetect
	au! BufRead tmux.config setfiletype tmux
	au! BufRead,BufNewFile *.zettel setfiletype zettel
	au! FileType qf map <silent> <buffer> dd :RemoveQFItem<cr>
	au! FileType gitcommit,gitrebase,gitconfig set bufhidden=delete
augroup END
