function gbnc -d 'Copy Git branch name to clipboard'
    set branch (git rev-parse --abbrev-ref HEAD)
    echo $branch
    echo $branch | tr -d '\n' | tr -d ' ' | pbcopy
end
