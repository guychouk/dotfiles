"""""""""""""""""""""""""
"      Statusline       "
"""""""""""""""""""""""""

let g:statusline_mode_map = {
			\ 'i'  : 'I',
			\ '!'  : '!',
			\ 't'  : 'B',
			\ 'R'  : 'R', 'Rv' : 'R',
			\ 'n'  : 'N',  'no' : 'N',
			\ 'v'  : 'V',  'V'  : 'V',  "\<C-V>" : 'V',
			\ 's'  : 'S',  'S'  : 'S',  "<\C-S>" : 'S',
			\ 'c'  : 'C', 'cv' : 'C', 'ce' : 'C',
			\ 'r'  : 'PROMPT',  'rm' : 'PROMPT',  'r?' : 'PROMPT',
			\ }

function! StatusLine(active)
	let l:clear = '%*'
	let l:statusline_mode_color_map = {
				\ 'i'  : '%3*',
				\ 'c'  : '%4*', 'cv' : '%4*', 'ce' : '%4*',
				\ 'v'  : '%5*',  'V'  : '%5*',  "\<C-V>" : '%5*',
				\ }
	let l:icon = a:active ? '(•◡•)' : '(ᴗ˳ᴗ)'
	let l:mode_color = a:active ? get(l:statusline_mode_color_map, mode(), '%1*') : '%2*'
	let l:filetype = empty(&filetype) ? '?' : '%{&filetype}'
	let l:fugitive = a:active && FugitiveHead() != '' ? '%3*●%* %{FugitiveHead()}' : ''
	let l:statusline_segments = [
				\ l:mode_color . '(%{g:statusline_mode_map[mode()]})' . l:clear,
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
		set noruler
		set noshowcmd
		set noshowmode
		set laststatus=0
	else
		set ruler
		set showcmd
		set showmode
		set laststatus=2
	endif
endfunction

nnoremap <plug>(ToggleStatusline) :call ToggleStatusline()<CR>
