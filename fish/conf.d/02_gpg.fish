if status is-interactive
  set -gx GPG_TTY (tty)
end

if command -q gpgconf
  set -gx SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket 2>/dev/null)
end
