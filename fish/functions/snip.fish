function snip -d "Select and insert snippet into command line"
    set -l snippets_file ~/dotfiles/SNIPPETS

    if test "$argv[1]" = "add"
        vim $snippets_file
        return
    end

    # Use fzf to select a snippet - preview shows the command
    set -l selected (
        awk '/^#/ {desc=substr($0, 3); getline; cmd=$0; print desc "\t" cmd}' $snippets_file | \
        fzf --delimiter='\t' --with-nth=1 \
            --preview="echo {2..} | bat -l bash -p --color always" \
            --preview-window=up:3:wrap
    )

    # If no selection, exit
    test -z "$selected"; and return 1

    # Extract command and insert into command line
    set -l cmd (echo $selected | cut -f2-)
    commandline -r $cmd
    commandline -f repaint
end
