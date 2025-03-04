set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif
let colors_name = "gman"

" General UI Elements
hi StatusLine   guifg=#4F5E62 guibg=#DDDECF gui=none ctermfg=188 ctermbg=240
hi StatusLineNC guifg=#4F5E62 guibg=#738A8B gui=none ctermfg=246 ctermbg=240
hi TabLine      guifg=#DDDECF guibg=#4F5E62 gui=none ctermfg=188 ctermbg=240 cterm=none
hi TabLineSel   guifg=#4F5E62 guibg=#DDDECF gui=none ctermfg=188 ctermbg=240 cterm=none
hi TabLineFill  guifg=#4F5E62 guibg=#4F5E62 gui=none ctermfg=188 ctermbg=240 cterm=none
hi Cursor       guifg=NONE    guibg=#738A8B gui=none ctermfg=253 ctermbg=246
hi CursorColumn guifg=NONE    guibg=#313B40 gui=none ctermbg=235
hi CursorLine   guifg=NONE    guibg=#313B40 gui=none ctermbg=235
hi CursorLineNr guifg=#E67E80 guibg=NONE    gui=bold ctermfg=203 ctermbg=NONE cterm=bold
hi FoldColumn                 guibg=NONE    ctermbg=NONE
hi Folded       guifg=#D6A0D1 guibg=NONE    gui=none ctermfg=176 ctermbg=4
hi LineNr       guifg=#617377 guibg=NONE    gui=none ctermfg=240 ctermbg=NONE
hi MatchParen   guifg=#B2C98F guibg=#313B40 gui=bold ctermfg=107 ctermbg=235 cterm=bold
hi NonText      guifg=#94AAA0 guibg=NONE    gui=none ctermfg=246 ctermbg=NONE
hi Normal       guifg=#DDDECF guibg=NONE    gui=none ctermfg=250 ctermbg=NONE
hi Pmenu        guifg=#DDDECF guibg=#232A2E gui=none ctermfg=250 ctermbg=235
hi PmenuSel     guifg=#000000 guibg=#B2C98F gui=none ctermfg=0 ctermbg=107
hi QuickFixLine guifg=#232A2E guibg=#DBBC7F gui=bold
hi SignColumn                 guibg=NONE    ctermbg=NONE
hi SpecialKey   guifg=#94AAA0 guibg=NONE    gui=none ctermfg=246 ctermbg=236
hi Title        guifg=#DDDECF guibg=NONE    gui=bold ctermfg=250 cterm=bold
hi VertSplit    guifg=#4F5E62 guibg=#4F5E62 gui=none ctermfg=240 ctermbg=240
hi Visual       guifg=#DBBC7F guibg=#4F5E62 gui=none ctermfg=179 ctermbg=4
hi qfFileName   guifg=#B2C98F guibg=NONE
hi qfLineNr     guifg=#94AAA0 guibg=NONE

" Diff Colors
hi DiffAdd      guifg=#B2C98F guibg=NONE gui=none ctermfg=107 ctermbg=22
hi DiffChange   guifg=#DBBC7F guibg=NONE gui=none ctermfg=179 ctermbg=54
hi DiffDelete   guifg=#E67E80 guibg=NONE gui=none ctermfg=203 ctermbg=52
hi DiffText     guifg=NONE    guibg=#4F5E62 gui=none ctermfg=240 ctermbg=237

" Syntax Highlighting
hi Boolean      guifg=#E67E80 guibg=NONE gui=none   ctermfg=203
hi Comment      guifg=#738A8B guibg=NONE gui=italic ctermfg=240
hi Constant     guifg=#E67E80 guibg=NONE gui=none   ctermfg=203
hi Function     guifg=#DDDECF guibg=NONE gui=none   ctermfg=250
hi Identifier   guifg=#E69875 guibg=NONE gui=none   ctermfg=203
hi Keyword      guifg=#E67E80 guibg=NONE gui=none   ctermfg=203
hi Number       guifg=#E67E80 guibg=NONE gui=none   ctermfg=203
hi PreProc      guifg=#DBBC7F guibg=NONE gui=none   ctermfg=179
hi Special      guifg=#E67E80 guibg=NONE gui=none   ctermfg=203
hi Statement    guifg=#D6A0D1 guibg=NONE gui=none   ctermfg=176
hi String       guifg=#B2C98F guibg=NONE gui=none   ctermfg=107
hi Todo         guifg=#E67E80 guibg=NONE gui=none   ctermfg=203
hi Type         guifg=#D6A0D1 guibg=NONE gui=none   ctermfg=176

" JSX/TSX tags highlighting
hi def link htmlTag     Comment
hi def link htmlTagName Identifier

" Plugins
hi DirvishPathTail guifg=#9BB5CF guibg=NONE gui=bold
hi FzfBorder       guifg=#B2C98F guibg=NONE gui=none
