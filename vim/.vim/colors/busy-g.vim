" This is my custom fork of the BusyBee.vim color scheme

set background=dark

hi clear

if exists("syntax_on")
  syntax reset
endif

let colors_name = "busy-g"

" Vim >= 7.0 specific colors
if version >= 700
  hi CursorLine                  guibg=#202020 gui=none             ctermbg=234
  hi CursorLineNr  guifg=#fabd2f guibg=NONE    gui=bold ctermfg=214 ctermbg=NONE cterm=bold
  hi CursorColumn                guibg=#202020 gui=none             ctermbg=234
  hi MatchParen    guifg=#d0ffc0 guibg=#202020 gui=bold ctermfg=157 ctermbg=237  cterm=bold
  hi Pmenu         guifg=#ffffff guibg=#202020 gui=none ctermfg=255 ctermbg=238
  hi PmenuSel      guifg=#000000 guibg=#b1d631 gui=none ctermfg=0   ctermbg=148
endif

" General colors
hi SignColumn      guibg=NONE    ctermbg=NONE
hi Cursor          guifg=NONE    guibg=#626262 gui=none               ctermbg=241
hi Normal          guifg=#e2e2e5 guibg=NONE    gui=none   ctermfg=253 ctermbg=NONE
hi NonText         guifg=#808080 guibg=NONE    gui=none   ctermfg=244 ctermbg=NONE
hi LineNr          guifg=#303030 guibg=NONE    gui=none   ctermfg=244 ctermbg=NONE
hi StatusLine      guifg=#d3d3d5 guibg=#303030 gui=none   ctermfg=253 ctermbg=238
hi StatusLineNC    guifg=#939395 guibg=#303030 gui=none   ctermfg=246 ctermbg=238
hi VertSplit       guifg=#444444 guibg=#303030 gui=none   ctermfg=238 ctermbg=238
hi Folded          guifg=#a0a8b0 guibg=#384048 gui=none   ctermfg=248 ctermbg=4
hi Title           guifg=#f6f3e8 guibg=NONE    gui=bold   ctermfg=254            cterm=bold
hi Visual          guifg=#faf4c6 guibg=#3c414c gui=none   ctermfg=254 ctermbg=4
hi SpecialKey      guifg=#808080 guibg=#343434 gui=none   ctermfg=244 ctermbg=236
hi QuickFixLine    guifg=#202020 guibg=#ffdd00 gui=bold
hi qfFileName      guifg=#b1d631 guibg=NONE
hi qfLineNr        guifg=#808080 guibg=NONE

" Syntax highlighting
hi Comment         guifg=#a0a0a0 guibg=NONE    gui=italic ctermfg=244
hi Boolean         guifg=#ffff00 guibg=NONE    gui=none   ctermfg=148
hi String          guifg=#b1d631 guibg=NONE    gui=none   ctermfg=148
hi Identifier      guifg=#ffff00 guibg=NONE    gui=none   ctermfg=148
hi Function        guifg=#f1bf98 guibg=NONE    gui=none   ctermfg=255
hi Type            guifg=#7e8aa2 guibg=NONE    gui=none   ctermfg=103
hi Statement       guifg=#7e8aa2 guibg=NONE    gui=none   ctermfg=103
hi Keyword         guifg=#ff9800 guibg=NONE    gui=none   ctermfg=208
hi Constant        guifg=#ff9800 guibg=NONE    gui=none   ctermfg=208
hi Number          guifg=#ff9800 guibg=NONE    gui=none   ctermfg=208
hi Special         guifg=#ff9800 guibg=NONE    gui=none   ctermfg=208
hi PreProc         guifg=#faf4c6 guibg=NONE    gui=none   ctermfg=230
hi Todo            guifg=#ff9f00 guibg=NONE    gui=none   ctermfg=208

" Plugin specific
hi FzfBorder       guifg=#98971C
hi GitGutterAdd    guifg=#009900 ctermfg=2 guibg=NONE ctermbg=NONE
hi GitGutterChange guifg=#bbbb00 ctermfg=3 guibg=NONE ctermbg=NONE
hi GitGutterDelete guifg=#ff2222 ctermfg=1 guibg=NONE ctermbg=NONE
hi DirvishPathTail guifg=#b1d631 guibg=NONE gui=bold

" Links
hi link GitGutterChangeDelete GitGutterChange
