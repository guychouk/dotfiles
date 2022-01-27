"""""""""""""""""""""""""
"       Utilities       "
"""""""""""""""""""""""""

function! SearchRange(type, ...)
	let sel_save = &selection
	let &selection = "inclusive"
	let reg_save = @@
	if a:0
		silent exe "normal! gvy"
	elseif a:type == 'line'
		silent exe "normal! '[V']y"
	else
		silent exe "normal! `[v`]y"
	endif
	exe "Rg" @@
	let &selection = sel_save
	let @@ = reg_save
endfunction

function! QuickfixRemoveEntry()
	let curqfidx = getqflist({ 'idx': 1 }).idx - 1
	let qfall = getqflist()
	call remove(qfall, curqfidx)
	call setqflist(qfall, 'r')
	if len(qfall) == 0
		silent ccl
		return
	endif
endfunction

function! ShowDocumentation()
	if (index(['vim','help'], &filetype) >= 0)
		exe 'h' expand('<cword>')
	endif
endfunction

function! ZoomToggle()
	if exists('t:zoomed') && t:zoomed
		execute t:zoom_winrestcmd
		let t:zoomed = 0
	else
		let t:zoom_winrestcmd = winrestcmd()
		resize
		vertical resize
		let t:zoomed = 1
	endif
endfunction

nnoremap <plug>(ZoomToggle)          :call      ZoomToggle()<CR>
nnoremap <plug>(ShowDocumentation)   :call      ShowDocumentation()<CR>
nnoremap <plug>(ShowDocumentation)   :call      ShowDocumentation()<CR>
nnoremap <plug>(QuickfixRemoveEntry) :call      QuickfixRemoveEntry()<CR>
vnoremap <plug>(SearchRange)         :<c-u>call SearchRange(visualmode(), 1)<CR>
