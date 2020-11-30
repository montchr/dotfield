function link_files -d 'Symlink a file to a destination and display a nice message'
    ln -sTf "$argv[1]" "$argv[2]"
    e_success "linked $argv[1] to $argv[2]"
end
