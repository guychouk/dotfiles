vim9script

g:statusline_mode_map = {
	'i': 'INSERT',
	'!': 'EXTERNAL',
	't': 'TERMINAL',
	'n': 'NORMAL',  'no': 'NORMAL',
	'R': 'REPLACE', 'Rv': 'REPLACE',
	'c': 'COMMAND', 'cv': 'COMMAND', 'ce': 'COMMAND',
	'r': 'PROMPT',  'rm': 'PROMPT',  'r?': 'PROMPT',
	'v': 'VISUAL',  'V':  'VISUAL',  "\<C-V>": 'VISUAL',
	's': 'SELECT',  'S':  'SELECT',  "<\C-S>": 'SELECT',
}

def g:BuildStatusline(active: bool): string
	var icon = active ? '(•◡•)' : '(ᴗ˳ᴗ)'
	var ft = empty(&filetype) ? '?' : '%{&filetype}'
	var fugitive = active && g:FugitiveHead() != '' ? '%{FugitiveHead()}' : ''
	var statusline_segments = [
		'%#StatusLineSection# %{g:statusline_mode_map[mode()]} %#StatusLineSep#',
		'%#StatusLine# ',
		fnamemodify(getcwd(), ':t'),
		' ',
		'%{expand("%")} %m %r %h',
		'%=',
		fugitive,
		'%#StatusLineSep#%#StatusLineSection#',
		ft,
		' ',
		icon 
	]
	return join(statusline_segments)
enddef

def g:ToggleStatusline()
	if &laststatus == 2
		set laststatus=0
	else
		set laststatus=2
	endif
enddef

augroup statusline
	autocmd!
	autocmd VimEnter * setlocal statusline=%!g:BuildStatusline(1)
	autocmd WinEnter * setlocal statusline=%!g:BuildStatusline(1)
	autocmd WinLeave * setlocal statusline=%!g:BuildStatusline(0)

augroup END

command! StatusLineToggle call g:ToggleStatusline()
