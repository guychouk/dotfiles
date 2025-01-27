set background=dark

hi clear

if exists("syntax_on")
  syntax reset
endif

let colors_name = "gman"

hi StatusLine   guifg=#3D3D3D guibg=#C3C3C3 gui=none ctermfg=253 ctermbg=238
hi StatusLineNC guifg=#3D3D3D guibg=#7A7A7A gui=none ctermfg=246 ctermbg=238

hi TabLine      guifg=#C3C3C3 guibg=#3D3D3D gui=none ctermfg=253 ctermbg=238 cterm=none
hi TabLineSel   guifg=#3D3D3D guibg=#C3C3C3 gui=none ctermfg=253 ctermbg=238 cterm=none
hi TabLineFill  guifg=#3D3D3D guibg=#3D3D3D gui=none ctermfg=253 ctermbg=238 cterm=none

hi Cursor       guifg=NONE    guibg=#5C5C5C gui=none ctermfg=253 ctermbg=241
hi CursorColumn guifg=NONE    guibg=#151515 gui=none ctermbg=234
hi CursorLine   guifg=NONE    guibg=#151515 gui=none ctermbg=234
hi CursorLineNr guifg=#FF8133 guibg=NONE    gui=bold ctermfg=214 ctermbg=NONE cterm=bold
hi FoldColumn                 guibg=NONE    ctermbg=NONE
hi Folded       guifg=#A0A8B0 guibg=NONE    gui=none ctermfg=248 ctermbg=4
hi LineNr       guifg=#303030 guibg=NONE    gui=none ctermfg=244 ctermbg=NONE
hi MatchParen   guifg=#D0FFC0 guibg=#151515 gui=bold ctermfg=157 ctermbg=237  cterm=bold
hi NonText      guifg=#808080 guibg=NONE    gui=none ctermfg=244 ctermbg=NONE
hi Normal       guifg=#F4F4F5 guibg=NONE    gui=none ctermfg=253 ctermbg=NONE
hi Pmenu        guifg=#FFFFFF guibg=#151515 gui=none ctermfg=255 ctermbg=238
hi PmenuSel     guifg=#000000 guibg=#B1D631 gui=none ctermfg=0   ctermbg=148
hi QuickFixLine guifg=#151515 guibg=#DACF62 gui=bold
hi SignColumn                 guibg=NONE    ctermbg=NONE
hi SpecialKey   guifg=#808080 guibg=NONE    gui=none ctermfg=244 ctermbg=236
hi Title        guifg=#F6F3E8 guibg=NONE    gui=bold ctermfg=254 cterm=bold
hi VertSplit    guifg=#3D3D3D guibg=#3D3D3D gui=none ctermfg=238 ctermbg=238
hi Visual       guifg=#FAF4C6 guibg=#3D3D3D gui=none ctermfg=254 ctermbg=4
hi qfFileName   guifg=#A0CC70 guibg=NONE
hi qfLineNr     guifg=#808080 guibg=NONE

" Diffs
function DiffMode()
  if &diff
    hi DiffAdd      guifg=#3D3D3D guibg=#A0CC70 gui=none ctermfg=2   ctermbg=NONE
    hi DiffChange   guifg=#3D3D3D guibg=#DACF62 gui=none ctermfg=3   ctermbg=NONE
    hi DiffDelete   guifg=#3D3D3D guibg=#DB6A61 gui=none ctermfg=1   ctermbg=NONE
    hi DiffText     guifg=NONE    guibg=#626DDA
  else
    hi DiffAdd      guifg=#A0CC70 guibg=NONE gui=none ctermfg=2   ctermbg=NONE
    hi DiffChange   guifg=#DACF62 guibg=NONE gui=none ctermfg=3   ctermbg=NONE
    hi DiffDelete   guifg=#DB6A61 guibg=NONE gui=none ctermfg=1   ctermbg=NONE
    hi DiffText     guifg=NONE    guibg=#626DDA
  endif
endfunction

autocmd BufEnter * call DiffMode()

" Syntax highlighting
hi Boolean    guifg=#FF8133 guibg=NONE gui=none   ctermfg=148
hi Comment    guifg=#707070 guibg=NONE gui=italic ctermfg=244
hi Constant   guifg=#FF8133 guibg=NONE gui=none   ctermfg=208
hi Function   guifg=#FFF2EB guibg=NONE gui=none   ctermfg=255
hi Identifier guifg=#FF8133 guibg=NONE gui=none   ctermfg=148
hi Keyword    guifg=#FF8133 guibg=NONE gui=none   ctermfg=208
hi Number     guifg=#FF8133 guibg=NONE gui=none   ctermfg=208
hi PreProc    guifg=#FAF4C6 guibg=NONE gui=none   ctermfg=230
hi Special    guifg=#FF8133 guibg=NONE gui=none   ctermfg=208
hi Statement  guifg=#C1C1C3 guibg=NONE gui=none   ctermfg=103
hi String     guifg=#A0CC70 guibg=NONE gui=none   ctermfg=148
hi Todo       guifg=#FF8133 guibg=NONE gui=none   ctermfg=208
hi Type       guifg=#C1C1C3 guibg=NONE gui=none   ctermfg=103

" Plugins
hi DirvishPathTail guifg=#93C6D6 guibg=NONE gui=bold
hi FzfBorder       guifg=#A0CC70 guibg=NONE gui=none
