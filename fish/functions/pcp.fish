function pcp --description "Copy pass entry password to clipboard, concealed from Maccy"
    pass show $argv[1] | head -1 | ~/scripts/pbcopy-concealed
    echo "Copied to clipboard, will clear in 45 seconds."
    command sleep 45 && echo "" | pbcopy &
    disown
end
