" Vim syntax file for SpiceDB/Zanzibar .zed files

if exists("b:current_syntax")
  finish
endif

syntax keyword zedKeyword definition relation permission
syntax match zedOperator /\(->\|[=|+\-*]\)/
syntax region zedComment start=+//+ end=+$+
syntax match zedBrace /[{}]/

" Highlighting
highlight link zedKeyword Keyword
highlight link zedOperator Operator
highlight link zedComment Comment
highlight link zedBrace Delimiter

let b:current_syntax = "zed"
