" Vim syntax file for SpiceDB/Zanzibar .zed files

if exists("b:current_syntax")
  finish
endif

syntax region zedComment start=+//+ end=+$+

syntax keyword zedKeyword definition relation permission

syntax match zedBrace    /[{}]/
syntax match zedOperator /\(->\|[=|+\-*]\)/

highlight link zedKeyword  Keyword
highlight link zedOperator Operator
highlight link zedComment  Comment
highlight link zedBrace    Delimiter

let b:current_syntax = "zed"
