if exists("did_load_filetypes")
	finish
endif

augroup filetypedetect
	au! BufNewFile,BufRead *.{frag}    setfiletype glsl
	au! BufNewFile,BufRead *.{repl}    setfiletype repl
	au! BufNewFile,BufRead tmux.config setfiletype tmux
augroup END
