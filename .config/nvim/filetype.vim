"""""""""""""""""""""""""""
"      Zettelkasten       "
"""""""""""""""""""""""""""

function! LinkZettel(val)
		let reg_save = @@
		silent exe "normal! gvy"
		silent exe "%s/".@@."/[".@@."]/g"
		silent exe "normal! Go\<Esc>o[".@@."]: ./".a:val."\<Esc>"
		let @@ = reg_save
endfunction

function! NewZettel()
	let l:zid = strftime("%Y%m%d%H%M%S")
	let l:title = input('Title: ')
	let l:filename = l:zid . '.md'
	let l:first_line = '#' . ' ' . substitute(l:title, '\<.', '\u&', 'g')
	call setline(1, l:first_line)
	exe 'silent w' l:filename
	exe 'Goyo'
	filetype detect
endfunction

function! SetupZettelkasten()
	set textwidth=80
	nnoremap <silent> <Leader>q :qa<CR>
	vnoremap <silent> <Leader>l :<c-u>call fzf#run(fzf#wrap({'sink': funcref('LinkZettel')}))<CR>
	nnoremap <silent> gl :exe "edit ".expand("**/".expand("<cword>")."**")<CR>
endfunction

""""""""""""""""
"   Quickfix   "
""""""""""""""""

function! RemoveLineFromQuickfix()
  let curqfidx = line('.') - 1
  let qfall = getqflist()
  call remove(qfall, curqfidx)
  call setqflist(qfall, 'r')
  execute curqfidx + 1 . "cfirst"
  :copen
endfunction

command! RemoveLineFromQuickfix :call RemoveLineFromQuickfix()

""""""""""""""""""""""""""""""""
"      Filetypes augroup       "
""""""""""""""""""""""""""""""""

if exists("did_load_filetypes")
	finish
endif
augroup filetypedetect
	au! FileType qf map <silent> <buffer> dd :RemoveLineFromQuickfix<CR>
	au! FileType gitcommit,gitrebase,gitconfig set bufhidden=delete

	au! BufRead tmux.config setfiletype tmux
	au! BufRead,BufNewFile NEW_ZETTEL call NewZettel()
	au! BufRead,BufNewFile **/zetz/*.md call SetupZettelkasten()
augroup END
