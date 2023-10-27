vim9script

import autoload 'microline.vim'

var microline_colors = {
	 'dark': {
		 'bg': '#1E1E1E',
		 'fg': '#c6c6c6',
		 'sep': '#c6c6c6',
		 'section': '#1E1E1E',
	 },
	 'light': {
		 'bg': '#c6c6c6',
		 'fg': '#1E1E1E',
		 'sep': '#1E1E1E',
		 'section': '#c6c6c6',
	 }
}

def SetupMicrolineColors()
	var colors = microline_colors[&background]
	exe "highlight MicrolineSep " .. "guibg=" .. colors.sep .. " guifg=" .. colors.section
	exe "highlight MicrolineSection " .. "guibg=" .. colors.section .. " guifg=NONE"
enddef

augroup Microline
	autocmd!
	autocmd ColorScheme   lunaperche  SetupMicrolineColors()

	autocmd WinEnter,BufEnter  *      setlocal statusline=%!microline#Build(v:true)
	autocmd WinLeave,BufLeave  *      setlocal statusline=%!microline#Build(v:false)
	autocmd FileType           *      setlocal statusline=%!microline#Build(v:true)
	autocmd BufFilePost        *      setlocal statusline=%!microline#Build(v:true)
	autocmd BufWritePost       *      setlocal statusline=%!microline#Build(v:true)
	autocmd OptionSet          buf*   setlocal statusline=%!microline#Build(v:true)
	autocmd VimResized         *      setlocal statusline=%!microline#Build(v:true)
	autocmd CmdWinEnter        *      setlocal statusline=\ Command\ Line\ %1*
augroup END

command! MicrolineToggle microline.Toggle()

SetupMicrolineColors()
