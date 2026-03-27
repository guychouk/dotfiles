if command -q fzf
  set -gx FZF_ALT_C_COMMAND ""
  set -gx FZF_DEFAULT_OPTS "--prompt='' --margin=2%,2% --height 65% --info=inline-right --layout=reverse --no-scrollbar --tiebreak=length"
  if status is-interactive
    fzf --fish | source
  end
end
