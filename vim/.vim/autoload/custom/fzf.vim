vim9script

def FugitiveCheckout(branch: string): void
  var track = match(branch, 'origin') != -1 ? '--track' : ''
  execute 'Git checkout ' .. track .. ' ' .. branch
enddef

export def GitCheckoutBranch(): void
  call fzf#run(
    fzf#wrap({
      'source':  'git branch -a --format "%(refname:short)"',
      'sink':    FugitiveCheckout,
      'options': '--prompt "Branch> "'
    })
  )
enddef

def InsertNoteLink(file: list<string>): string
  if len(file) != 0
    var ext = expand('%:e')
    var new_ext = '.html'
    var modified_file = substitute(substitute(file[0], '\.' .. ext .. '$', new_ext, ''), '\.', '', '')
    return modified_file
  endif
  return ''
enddef

export def CompleteNoteLink(): string
  return fzf#vim#complete(
    fzf#wrap({
      'source':  'find ./notes -name "*.md"',
      'reducer': InsertNoteLink,
      'options': '--reverse',
      'window': { 'width': 0.4, 'height': 0.7 } 
    })
  )
enddef
