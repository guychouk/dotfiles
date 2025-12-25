function fish_prompt
    # Set cursor to blinking underline
    echo -ne '\e[3 q'

    # Hostname
    echo -ne '\e[38;5;8m'
    set -l host_name (prompt_hostname)
    set -l host_parts (string split '-' $host_name)
    echo -n $host_parts[1]' '
    echo -ne '\e[0m'

    # Current directory (last 2 components, full names)
    echo -ne '\e[38;5;4m'
    set -l pwd_parts (string split / (prompt_pwd --dir-length=0))
    if test (count $pwd_parts) -gt 2
        echo -n $pwd_parts[-2]/$pwd_parts[-1]
    else
        echo -n (prompt_pwd --dir-length=0)
    end
    echo -ne '\e[0m'

    # Git branch
    if git rev-parse HEAD >/dev/null 2>&1
        set git_branch (git symbolic-ref --short HEAD 2>/dev/null)
        if test -n "$git_branch"
            # Split branch name by dashes and take first two parts
            set -l branch_parts (string split '-' $git_branch)
            if test (count $branch_parts) -gt 2
                set git_branch "$branch_parts[1]-$branch_parts[2]"
            end
            echo -ne '\e[38;5;7m'
            echo -n " $git_branch"
            echo -ne '\e[0m'
        end
    end

    # Lambda
    echo -ne '\e[38;5;3m'
    echo -n ' λ '
    echo -ne '\e[0m'
end
