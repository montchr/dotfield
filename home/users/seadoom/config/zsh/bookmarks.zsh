# -*- sh -*-
#
# SPDX-FileCopyrightText: 2021 Vincent Bernat <vincent@bernat.ch>
# SPDX-License-Identifier: CC0-1.0
#
# https://github.com/vincentbernat/zshrc/blob/e96ea1ee3c5b11cda23139dfdb93ee0c9f33c275/rc/bookmarks.zsh

# Handle bookmarks. This uses the static named directories feature of
# zsh. Such directories are declared with `hash -d
# name=directory`. Both prompt expansion and completion know how to
# handle them. We populate the hash with directories.
#
# With autocd, you can just type `~-bookmark`. Since this can be
# cumbersome to type, you can also type `@@` and this will be turned
# into `~-` by ZLE.

() {
    typeset -g MARKPATH=$ZSH/run/marks

    # Add some static entries
    hash -d log=/var/log
    hash -d doc=/usr/share/doc

    # Populate the hash
    local link
    for link ($MARKPATH/*(-N/)) {
        hash -d -- -${link:t}=${link:A}
    }

    vbe-insert-bookmark() {
        emulate -L zsh
        LBUFFER=${LBUFFER}"~-"
    }
    zle -N vbe-insert-bookmark
    bindkey '@@' vbe-insert-bookmark

    # Manage bookmarks
    bookmark() {
        local link
        [[ -d $MARKPATH ]] || mkdir -p $MARKPATH
        if (( $# == 0 )); then
            # When no arguments are provided, just display existing
            # bookmarks
            for link in $MARKPATH/*(N@); do
                local markname=${(%):-%F{green}${link:t}%f}
                local markpath=${(%):-%F{blue}${link:A}%f}
                printf "%-30s → %s\n" $markname $markpath
            done
        else
            # Otherwise, we may want to add a bookmark or delete an
            # existing one.
            local -a delete
            zparseopts -D d=delete
            if (( $+delete[1] )); then
                # With `-d`, we delete an existing bookmark
                command rm $MARKPATH/$1
            else
                # Otherwise, add a bookmark to the current
                # directory. The first argument is the bookmark
                # name. `.` is special and means the bookmark should
                # be named after the current directory.
                local name=$1
                [[ $name == "." ]] && name=${PWD:t}
                ln -sf $PWD $MARKPATH/$name
                hash -d -- -${name}=${PWD}
            fi
        fi
    }
}
