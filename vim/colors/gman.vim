set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif
let colors_name = "gman"

" General UI Elements
hi StatusLine   guifg=#4f5e62 guibg=#dddecf gui=none ctermfg=188 ctermbg=240
hi StatusLineNC guifg=#4f5e62 guibg=#738a8b gui=none ctermfg=246 ctermbg=240
hi TabLine      guifg=#dddecf guibg=#4f5e62 gui=none ctermfg=188 ctermbg=240 cterm=none
hi TabLineSel   guifg=#4f5e62 guibg=#dddecf gui=none ctermfg=188 ctermbg=240 cterm=none
hi TabLineFill  guifg=#4f5e62 guibg=#4f5e62 gui=none ctermfg=188 ctermbg=240 cterm=none
hi Cursor       guifg=NONE    guibg=#738a8b gui=none ctermfg=253 ctermbg=246
hi CursorColumn guifg=NONE    guibg=#313b40 gui=none ctermbg=235
hi CursorLine   guifg=NONE    guibg=#313b40 gui=none ctermbg=235
hi CursorLineNr guifg=#e67e80 guibg=NONE    gui=bold ctermfg=203 ctermbg=NONE cterm=bold
hi FoldColumn                 guibg=NONE    ctermbg=NONE
hi Folded       guifg=#d6a0d1 guibg=NONE    gui=none ctermfg=176 ctermbg=4
hi LineNr       guifg=#617377 guibg=NONE    gui=none ctermfg=240 ctermbg=NONE
hi MatchParen   guifg=#b2c98f guibg=#313b40 gui=bold ctermfg=107 ctermbg=235 cterm=bold
hi NonText      guifg=#4b5952 guibg=NONE    gui=none ctermfg=246 ctermbg=NONE
hi SpecialKey   guifg=#4b5952 guibg=NONE    gui=none ctermfg=246 ctermbg=236
hi Normal       guifg=#dddecf guibg=NONE    gui=none ctermfg=250 ctermbg=NONE
hi Pmenu        guifg=#dddecf guibg=#232a2e gui=none ctermfg=250 ctermbg=235
hi PmenuSel     guifg=#000000 guibg=#b2c98f gui=none ctermfg=0 ctermbg=107
hi QuickFixLine guifg=#232a2e guibg=#dbbc7f gui=bold
hi SignColumn                 guibg=NONE    ctermbg=NONE
hi Title        guifg=#dddecf guibg=NONE    gui=bold ctermfg=250 cterm=bold
hi VertSplit    guifg=#4f5e62 guibg=#4f5e62 gui=none ctermfg=240 ctermbg=240
hi Visual       guifg=#dbbc7f guibg=#4f5e62 gui=none ctermfg=179 ctermbg=4
hi qfFileName   guifg=#b2c98f guibg=NONE
hi qfLineNr     guifg=#94aaa0 guibg=NONE

" Diff Colors
hi DiffAdd      guifg=#b2c98f guibg=NONE gui=none ctermfg=107 ctermbg=22
hi DiffChange   guifg=#dbbc7f guibg=NONE gui=none ctermfg=179 ctermbg=54
hi DiffDelete   guifg=#e67e80 guibg=NONE gui=none ctermfg=203 ctermbg=52
hi DiffText     guifg=NONE    guibg=#4f5e62 gui=none ctermfg=240 ctermbg=237

" Syntax Highlighting
hi Boolean      guifg=#e67e80 guibg=NONE gui=none   ctermfg=203
hi Comment      guifg=#738a8b guibg=NONE gui=italic ctermfg=240
hi Constant     guifg=#e67e80 guibg=NONE gui=none   ctermfg=203
hi Function     guifg=#dddecf guibg=NONE gui=none   ctermfg=250
hi Identifier   guifg=#e69875 guibg=NONE gui=none   ctermfg=203
hi Keyword      guifg=#e67e80 guibg=NONE gui=none   ctermfg=203
hi Number       guifg=#e67e80 guibg=NONE gui=none   ctermfg=203
hi PreProc      guifg=#dbbc7f guibg=NONE gui=none   ctermfg=179
hi Special      guifg=#e67e80 guibg=NONE gui=none   ctermfg=203
hi Statement    guifg=#d6a0d1 guibg=NONE gui=none   ctermfg=176
hi String       guifg=#b2c98f guibg=NONE gui=none   ctermfg=107
hi Todo         guifg=#e67e80 guibg=NONE gui=none   ctermfg=203
hi Type         guifg=#d6a0d1 guibg=NONE gui=none   ctermfg=176

" JSX/TSX tags highlighting
hi def link htmlTag     Comment
hi def link htmlTagName Identifier

" Plugins
hi DirvishPathTail guifg=#9bb5cf guibg=NONE gui=bold
hi FzfBorder       guifg=#b2c98f guibg=NONE gui=none
