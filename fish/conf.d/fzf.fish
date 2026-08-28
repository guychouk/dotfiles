if command -q fzf
  set -gx FZF_ALT_C_COMMAND ""
  set -gx FZF_DEFAULT_OPTS "--margin=2%,0% --height 70% --info=hidden --layout=reverse --no-scrollbar"
  if status is-interactive
    fzf --fish | source
  end
end
