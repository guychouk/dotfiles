"""""""""""""""""""""""""""
"      Zettelkasten       "
"""""""""""""""""""""""""""

function! LinkZettel(val)
		let zid = split(a:val, '-')[0]
		execute "normal! i[".zid."]\<Esc>"
endfunction

function! SetupZettelkasten()
	set textwidth=80
	nnoremap <silent> <Leader>q :qa<CR>
	nnoremap <silent> <Leader>l :call fzf#run(fzf#wrap({'sink': funcref('LinkZettel')}))<CR>
	nnoremap <silent> gf :exe "edit ".expand("**/".expand("<cword>")."**")<CR>
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

command! RemoveLineFromQuickfix :call RemoveFromQuickfix()

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
	au! BufRead,BufNewFile **/zetz/*.md call SetupZettelkasten()
augroup END
