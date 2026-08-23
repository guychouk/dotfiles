set -g fish_greeting         ""
set -g fish_color_error      red
set -g fish_color_quote      green
set -g fish_color_command    green
set -g fish_color_param      normal
set -g fish_color_valid_path --underline

set -g fish_history_max 50000
function save_history --on-event fish_postexec
  history save
end
