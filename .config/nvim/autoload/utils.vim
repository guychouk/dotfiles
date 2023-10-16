function! utils#HandleTab() abort
	let line = getline('.')
	let substr = strpart(line, 0, col('.') - 1)
	let is_preceded_by_dot = match(substr, '\.') != -1
	let is_cursor_on_first_column = col('.') == 1
	let has_preceding_whitespace = line[col('.') - 2]  =~# '\s'
	if (is_cursor_on_first_column || has_preceding_whitespace)
		return "\<Tab>"
	elseif (vsnip#expandable())
		return "\<C-j>"
	else
		return "\<C-N>"
	endif
endfunction

function! utils#ZoomPane() abort
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

function! utils#VimHelp() abort
	if (index(['vim','help'], &filetype) >= 0)
		exe 'h' expand('<cword>')
	endif
endfunction

function! utils#RemoveQfItem() abort
	let l:qf_list = getqflist()
	if len(l:qf_list) <= 1
		cclose
		return
	endif
	let l:curqfidx = line('.') - 1
	call remove(l:qf_list, curqfidx)
	call setqflist(l:qf_list, 'r')
	execute l:curqfidx + 1 . 'cfirst'
	copen
endfunction

function! utils#GetVisualSelection() abort
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

function! utils#FzfExcludeFilenamesFromRg(qargs, fullscreen) abort
	let l:rg_cmd = 'rg --column --line-number --no-heading --color=always --smart-case '. a:qargs
	return fzf#vim#grep(l:rg_cmd, 1, { 'options': '--delimiter : --nth 4..' }, a:fullscreen)
endfunction

function! utils#FzfCompleteSnippet(...) abort
	return fzf#vim#complete(fzf#wrap({
				\ 'source':  "cat " . $HOME . "/.config/nvim/snippets/" . &ft . ".json" . " | jq -r 'to_entries[] | \"\\(.key): \\(.value.prefix)\"'",
				\ 'reducer': { lines -> trim(split(lines[0], ':')[1]) } }))
endfunction

function! utils#GitCheckoutBranch(branch) abort
	let l:track = match(a:branch, 'origin') != -1 ? '--track' : ''
	execute 'Git checkout ' . l:track . ' ' . a:branch
endfunction

function! utils#FzfGitCheckoutBranch() abort
	call fzf#run(fzf#wrap({
				\ 'source':  'git branch -a --format "%(refname:short)"',
				\ 'sink':    function('utils#GitCheckoutBranch'),
				\ 'options': '--prompt "Branch> "' }))
endfunction

function! utils#VimuxSlimeSelection() abort
	call VimuxRunCommand(@v)
endfunction

function! utils#VimuxSlimeLine() abort
	let rv = getreg('"')
	let rt = getregtype('"')
	execute "normal! yy"
	call VimuxRunCommand(trim(@"))
	call setreg('"', rv, rt)
endfunction

function! utils#GoyoEnter() abort
	silent !tmux set status off
	silent !tmux list-panes -F '\#F' | grep -q Z || tmux resize-pane -Z
endfunction

function! utils#GoyoLeave() abort
	silent !tmux set status on
	silent !tmux list-panes -F '\#F' | grep -q Z && tmux resize-pane -Z
endfunction

