" Vim syntax file for my Zettels

if exists("b:current_syntax")
  finish
endif

let b:current_syntax = "zettel"

syn match linksMatch '\[.*\]'
syn region metadataRegion start='---' end='---' keepend

hi def link linksMatch              Comment
hi def link metadataRegion          Comment
