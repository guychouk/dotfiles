function pcp --description "Copy pass entry password to clipboard, concealed from Maccy"
    pass show $argv[1] | head -1 | ~/scripts/pbcopy-concealed
    echo "Copied."
end
