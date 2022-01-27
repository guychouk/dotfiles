"""""""""""""""""""""""""
"       SmartTab        "
"""""""""""""""""""""""""

function smarttab#complete()
	let fn_fzf_file_match = "fzf#vim#complete#path(\"find . -path '*/\.*' -prune -o -type f -print -o -type l -print \| sed 's:^..::'\")"

	let line = getline('.')

	" from the start of the current line to one character right of the cursor
	let substr = strpart(line, -1, col('.')+1)
	" word till cursor
	let substr = matchstr(substr, "[^ \t]*$")

	" insert tab when:
	" - substr is empty
	" - there's preceding whitespace
	" - the cursor is on the first column
	if (strlen(substr) == 0 || col('.') == 1 || line[col('.') - 2]  =~# '\s')
		return "\<tab>"
	endif

	" position of slash, if any
	let has_slash = match(substr, '\/') != -1
	" position of period, if any
	let has_period = match(substr, '\.') != -1

	" existing text matching
	if (!has_period && !has_slash)
		return "\<C-X>\<C-P>"
	elseif (has_slash)
		" file matching
		return exists(":FZF") ? "\<C-\>\<C-O>:call " . fn_fzf_file_match . "\<CR>" : "\<C-X>\<C-F>"
	else
		" plugin matching
		return "\<C-X>\<C-O>"
	endif
endfunction

inoremap <expr> <plug>(SmartTabComplete) smarttab#complete()
