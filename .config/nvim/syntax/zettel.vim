" Vim syntax file for my Zettels

if exists("b:current_syntax")
  finish
endif

syn match linksMatch '\[.*\]'

syn keyword metadataKeywords id title

syn region metadataRegion start='---' end='---' keepend

let b:current_syntax = "zettel"

hi def link linksMatch              Comment
hi def link metadataKeywords        Todo
hi def link metadataRegion          Comment
