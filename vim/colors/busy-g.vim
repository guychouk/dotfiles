" This is my custom fork of the BusyBee.vim color scheme

set background=dark

hi clear

if exists("syntax_on")
  syntax reset
endif

let colors_name = "busy-g"

if version >= 700
  hi CursorLine                  guibg=#202020 gui=none             ctermbg=234
  hi CursorLineNr  guifg=#ff9800 guibg=NONE    gui=bold ctermfg=214 ctermbg=NONE cterm=bold
  hi CursorColumn  guifg=NONE    guibg=#202020 gui=none             ctermbg=234
  hi MatchParen    guifg=#d0ffc0 guibg=#202020 gui=bold ctermfg=157 ctermbg=237  cterm=bold
  hi Pmenu         guifg=#ffffff guibg=#202020 gui=none ctermfg=255 ctermbg=238
  hi PmenuSel      guifg=#000000 guibg=#b1d631 gui=none ctermfg=0   ctermbg=148
endif

" General
hi SignColumn      guibg=NONE    ctermbg=NONE
hi Cursor          guifg=NONE    guibg=#626262 gui=none               ctermbg=241
hi Normal          guifg=#e2e2e5 guibg=NONE    gui=none   ctermfg=253 ctermbg=NONE
hi NonText         guifg=#808080 guibg=NONE    gui=none   ctermfg=244 ctermbg=NONE
hi LineNr          guifg=#303030 guibg=NONE    gui=none   ctermfg=244 ctermbg=NONE

if has('gui_running')
  hi StatusLine    guifg=#c3c3c3 guibg=#303030 gui=none   ctermfg=244 ctermbg=238
else
  hi StatusLine    guifg=#303030 guibg=#c3c3c3 gui=none   ctermfg=253 ctermbg=238
endif

hi StatusLineNC    guifg=#303030 guibg=#909090 gui=none   ctermfg=246 ctermbg=238
hi VertSplit       guifg=#444444 guibg=#303030 gui=none   ctermfg=238 ctermbg=238
hi Folded          guifg=#a0a8b0 guibg=NONE gui=none   ctermfg=248 ctermbg=4
hi Title           guifg=#f6f3e8 guibg=NONE    gui=bold   ctermfg=254            cterm=bold
hi Visual          guifg=#faf4c6 guibg=#3c414c gui=none   ctermfg=254 ctermbg=4
hi SpecialKey      guifg=#808080 guibg=NONE    gui=none   ctermfg=244 ctermbg=236
hi QuickFixLine    guifg=#202020 guibg=#ffdd00 gui=bold
hi qfFileName      guifg=#b1d631 guibg=NONE
hi qfLineNr        guifg=#808080 guibg=NONE
hi DiffText        guifg=NONE    guibg=#aa0000
hi DiffDelete      guifg=#ff0000 guibg=NONE gui=none ctermbg=NONE ctermfg=1
hi DiffAdd         guifg=#00ff00 guibg=NONE gui=none ctermbg=NONE ctermfg=2
hi DiffChange      guifg=#ffff00 guibg=NONE gui=none ctermbg=NONE ctermfg=3

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
hi Todo            guifg=#ff9800 guibg=NONE    gui=none   ctermfg=208

" Plugins
hi FzfBorder       guifg=#b1d631 guibg=NONE gui=none
hi DirvishPathTail guifg=#93c6d6 guibg=NONE gui=bold
