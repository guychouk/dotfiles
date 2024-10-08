setlocal foldlevel=99
setlocal suffixesadd=.js,.jsx,.ts,.tsx
setlocal path+=src/**,static/,config/

compiler eslint

map <buffer> <localleader>r :Dispatch -compiler=make node %<CR>

EmmetInstall
