"""""""""""""""""""""""""
"      Statusline       "
"""""""""""""""""""""""""

let g:statusline_mode_map = {
			\ 'i'  : 'INSERT',
			\ '!'  : 'EXTERNAL',
			\ 't'  : 'TERMINAL',
			\ 'n'  : 'NORMAL',  'no' : 'NORMAL',
			\ 'R'  : 'REPLACE', 'Rv' : 'REPLACE',
			\ 'c'  : 'COMMAND', 'cv' : 'COMMAND', 'ce' : 'COMMAND',
			\ 'r'  : 'PROMPT',  'rm' : 'PROMPT',  'r?' : 'PROMPT',
			\ 'v'  : 'VISUAL',  'V'  : 'VISUAL',  "\<C-V>" : 'VISUAL',
			\ 's'  : 'SELECT',  'S'  : 'SELECT',  "<\C-S>" : 'SELECT',
			\ }

function! StatusLine(active)
	let l:icon = a:active ? '(•◡•)' : '(ᴗ˳ᴗ)'
	let l:filetype = empty(&filetype) ? '?' : '%{&filetype}'
	let l:fugitive = a:active && FugitiveHead() != '' ? '%{FugitiveHead()}' : ''
	let l:statusline_segments = [
				\ ' %{g:statusline_mode_map[mode()]}',
				\ '   ',
				\ fnamemodify(getcwd(), ':t'),
				\ '   ',
				\ '%{expand("%")} %m %r %h',
				\ '%=',
				\ l:fugitive,
				\ '   ',
				\ l:filetype,
				\ '   ',
				\ l:icon 
				\]
	return join(l:statusline_segments)
endfunction

function! ToggleStatusline()
	if &laststatus == 2
		set laststatus=0
	else
		set laststatus=2
	endif
endfunction

nnoremap <plug>(ToggleStatusline) :call ToggleStatusline()<CR>
