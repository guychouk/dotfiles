function ssh --wraps ssh --description "kitten ssh when local in kitty, so launch --cwd=current reconnects"
  if set -q KITTY_WINDOW_ID; and not set -q SSH_CONNECTION; and type -q kitten
    kitten ssh $argv
  else
    command ssh $argv
  end
end
