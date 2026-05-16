syn clear cDefine
syn match cDefine display "^\s*\zs\%(%:\|#\)\s*\%(define\|undef\)\>"
syn match cInclude display "^\s*\zs#\s*import\>"
hi link cInclude Keyword

syn keyword cMemFunctions memcpy memmove memset memcmp memccpy memchr memscan malloc calloc realloc free
hi link cMemFunctions Keyword
