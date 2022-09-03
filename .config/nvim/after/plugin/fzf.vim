" Exclude file names from Rg matches
command! -bang -nargs=* Rg call fzf#vim#grep("rg --column --line-number --no-heading --color=always --smart-case ".<q-args>, 1, {'options': '--delimiter : --nth 4..'}, <bang>0) 

function! FzfCompleteSnippet(...)
  return fzf#vim#complete(fzf#wrap({
  \ 'source':  "cat " . $HOME . "/.config/nvim/snippets/" . &ft . ".json" . " | jq -r 'to_entries[] | \"\\(.key): \\(.value.prefix)\"'",
  \ 'reducer': { lines -> trim(split(lines[0], ':')[1]) } }))
endfunction

inoremap <expr> <plug>(custom-fzf-complete-snippet) FzfCompleteSnippet()
