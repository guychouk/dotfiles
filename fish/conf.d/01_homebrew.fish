if test (uname) = "Darwin" -a -f /opt/homebrew/bin/brew
  set -gx HOMEBREW_CASK_OPTS "--appdir=/Applications"
  eval (/opt/homebrew/bin/brew shellenv)
end
