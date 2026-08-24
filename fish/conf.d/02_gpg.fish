if status is-interactive
  set -gx GPG_TTY (tty)
end

if command -q gpgconf
  set -gx SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket 2>/dev/null)
end

# auto-switch pinentry mode: curses over SSH, GUI locally
if test -n "$SSH_TTY"
  pinentry-mode ssh >/dev/null 2>&1
else
  pinentry-mode gui >/dev/null 2>&1
end
