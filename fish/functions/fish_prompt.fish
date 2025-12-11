function fish_prompt
    # Set cursor to blinking underline
    echo -ne '\e[3 q'

    # Hostname
    echo -ne '\e[38;5;8m'
    echo -n (prompt_hostname)' '
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
            echo -ne '\e[38;5;180m'
            echo -n " $git_branch"
            echo -ne '\e[0m'
        end
    end

    # Lambda
    echo -ne '\e[38;5;11m'
    echo -n ' λ '
    echo -ne '\e[0m'
end
