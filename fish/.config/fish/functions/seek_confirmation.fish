function seek_confirmation -d 'Ask for confirmation before proceeding'
    printf "\n"
    e_warning "$argv"
    printf "Continue? (y/n) " && read -n 1
    printf "\n"
end
