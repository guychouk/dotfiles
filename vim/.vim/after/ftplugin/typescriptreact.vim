setlocal tabstop=4
setlocal shiftwidth=4
setlocal foldlevel=99
setlocal suffixesadd=.js,.jsx,.ts,.tsx

compiler tsc

map <buffer> <localleader>e :Dispatch -compiler=eslint<CR>

EmmetInstall
