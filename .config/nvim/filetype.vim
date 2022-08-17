if exists("did_load_filetypes")
	finish
endif

augroup filetypedetect
	au! BufNewFile,BufRead tmux.config setfiletype tmux
	au! BufNewFile,BufRead *.{repl}    setfiletype repl
	au! BufNewFile,BufRead *.vert,*.tesc,*.tese,*.glsl,*.geom,*.frag,*.comp,*.rgen,*.rmiss,*.rchit,*.rahit,*.rint,*.rcall set filetype=glsl
augroup END
