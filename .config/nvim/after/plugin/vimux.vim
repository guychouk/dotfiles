" Slime-like plugin built on top of Vimux

function! s:get_visual_selection()
	let [line_start, column_start] = getpos("'<")[1:2]
	let [line_end, column_end] = getpos("'>")[1:2]
	let lines = getline(line_start, line_end)
	if len(lines) == 0
		return ''
	endif
	let lines[-1] = lines[-1][: column_end - (&selection == 'inclusive' ? 1 : 2)]
	let lines[0] = lines[0][column_start - 1:]
	return lines
endfunction

function! VimuxSlime()
	let lines = s:get_visual_selection()
	for line in lines
		sleep 50m
		call VimuxRunCommand(line)
	endfor
endfunction

function! VimuxSlimeLine()
	let rv = getreg('"')
	let rt = getregtype('"')
	execute "normal! yy"
	call VimuxRunCommand(trim(@"))
	call setreg('"', rv, rt)
endfunction

vnoremap <plug>(VimuxSlime)     :<c-u>call VimuxSlime()<CR>
nnoremap <plug>(VimuxSlimeLine) :call VimuxSlimeLine()<CR>
