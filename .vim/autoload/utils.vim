vim9script

export def Highlight(): void
	redir @a | highlight | redir END | new | put a
enddef

export def HandleTab(): string
	var line = getline('.')
	var substr = strpart(line, 0, col('.') - 1)
	var is_preceded_by_dot = match(substr, '\.') != -1
	var is_cursor_on_first_column = col('.') == 1
	var has_preceding_whitespace = line[col('.') - 2]  =~# '\s'
	if (is_cursor_on_first_column || has_preceding_whitespace)
		return "\<Tab>"
	elseif (vsnip#expandable())
		return "\<C-j>"
	else
		return "\<C-N>"
	endif
enddef

export def ZoomPane(): void
	if exists('t:zoomed') && t:zoomed
		execute t:zoom_winrestcmd
		t:zoomed = 0
	else
		t:zoom_winrestcmd = winrestcmd()
		resize
		vertical resize
		t:zoomed = 1
	endif
enddef

export def VimHelp(): void
    if index(['vim', 'help'], &filetype) >= 0
	execute 'help' expand('<cword>')
    endif
enddef

export def RemoveQfItem(): void
	var qf_list = getqflist()
	if len(qf_list) <= 1
		cclose
		return
	endif
	var curqfidx = line('.') - 1
	var nextqfidx = curqfidx + 1
	call remove(qf_list, curqfidx)
	call setqflist(qf_list, 'r')
	execute ':' .. nextqfidx .. 'cfirst'
	copen
enddef

export def FzfExcludeFilenamesFromRg(qargs: string, fullscreen: bool): void
	var rg_cmd = 'rg --column --line-number --no-heading --color=always --smart-case ' .. qargs
	call fzf#vim#grep(rg_cmd, 1, { 'options': '--delimiter : --nth 4..' }, fullscreen)
enddef

export def FzfCompleteSnippet(): string
	return fzf#vim#complete(
		fzf#wrap({
			'source':  "cat " .. $HOME .. "/.config/nvim/snippets/" .. &ft .. ".json" .. " | jq -r 'to_entries[] | \"\\(.key): \\(.value.prefix)\"'",
			'reducer': (lines) => trim(split(lines[0], ':')[1])
		})
	)
enddef

export def GitCheckoutBranch(branch: string): void
	var track = match(branch, 'origin') != -1 ? '--track' : ''
	execute 'Git checkout ' .. track .. ' ' .. branch
enddef

export def FzfGitCheckoutBranch(): void
	call fzf#run(fzf#wrap({
				\ 'source':  'git branch -a --format "%(refname:short)"',
				\ 'sink':    function('utils#GitCheckoutBranch'),
				\ 'options': '--prompt "Branch> "' }))
enddef

export def InsertNoteLink(file: list<string>): string
	if len(file) != 0
		var ext = expand('%:e')
		var new_ext = '.html'
		var modified_file = substitute(substitute(file[0], '\.' .. ext .. '$', new_ext, ''), '\.', '', '')
		return modified_file
	endif
	return ''
enddef

export def FzfCompleteNoteLink(): string
	return fzf#vim#complete(
		fzf#wrap({
			'source':  'find ./notes -name "*.md"',
			'reducer': function('utils#InsertNoteLink'),
			'options': '--reverse',
			'window': { 'width': 0.4, 'height': 0.7 } 
		})
	)
enddef

export def GoyoEnter(): void
	silent !tmux set status off
	silent !tmux list-panes -F '\#F' | grep -q Z || tmux resize-pane -Z
enddef

export def GoyoLeave(): void
	silent !tmux set status on
	silent !tmux list-panes -F '\#F' | grep -q Z && tmux resize-pane -Z
enddef

export def ReadQfListFromFile(filepath: string): void
  var qf_list = []
  for line in readfile(filepath)
    var parts = split(line, ':')
    if len(parts) >= 3
      var filename = parts[0]
      var lnum = str2nr(parts[1])
      var col = str2nr(parts[2])
      var message = len(parts) >= 4 ? parts[3] : ''
      call add(qf_list, {'filename': filename, 'lnum': lnum, 'col': col, 'text': message})
    endif
  endfor
  call setqflist(qf_list)
  copen
enddef
