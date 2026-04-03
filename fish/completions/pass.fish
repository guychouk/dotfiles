# Fish completion script for pass
complete -c pass -f -n 'not __fish_seen_subcommand_from show otp edit generate rm mv cp git init ls find grep' -a 'show otp edit generate rm mv cp git init ls find grep'
complete -c pass -f -n 'not __fish_seen_subcommand_from show otp edit generate rm mv cp git init ls find grep' -a "(pass_entries)"
complete -c pass -f -n '__fish_seen_subcommand_from show otp edit rm mv cp generate' -a "(pass_entries)"
