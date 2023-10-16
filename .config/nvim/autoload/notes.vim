function! notes#InsertNoteLink(file) abort
	if len(a:file) != 0
		let ext = expand('%:e')
		let new_ext = '.html'
		let modified_file = substitute(substitute(a:file[0], '\.'.ext.'$', new_ext, ''), '\.', '', '')
		return modified_file
	endif
endfunction

function! notes#FzfCompleteNoteLink(...) abort
	return fzf#vim#complete(fzf#wrap({
				\ 'source':  'find ./notes -name "*.md"',
				\ 'reducer': function('notes#InsertNoteLink'),
				\ 'options': '--reverse',
				\ 'window': { 'width': 0.4, 'height': 0.7 } }))
endfunction
