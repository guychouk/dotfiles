if command -q direnv
  set -gx DIRENV_LOG_FORMAT ""
  direnv hook fish | source
end
