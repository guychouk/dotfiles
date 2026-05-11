function snip -d "Select and insert snippet into command line using fzf"
  set -l selected (
  awk '/^#/ {desc=substr($0, 3); getline; cmd=$0; print desc "\t" cmd}' $SNIPPETS_FILE | \
    fzf --delimiter='\t' --with-nth=1 \
    --preview="echo {2..} | bat -l bash -p --color always" \
    --preview-window=up:3:wrap
  )
  test -z "$selected"; and return 1
  set -l cmd (echo $selected | cut -f2-)
  commandline -r $cmd
  commandline -f repaint
end
