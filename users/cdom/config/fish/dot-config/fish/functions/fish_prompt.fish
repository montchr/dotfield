function fish_prompt
    set -l last_status $status

    set -l color_primary blue
    set -l color_info cyan
    set -l color_mute brblack
    set -l color_success magenta
    set -l color_normal normal
    set -l color_danger red
    set -l color_light white
    set -l color_warn yellow
    set -l color_dark black
    set -l color_cwd $fish_color_cwd

    if fish_is_root_user
        set color_cwd $fish_color_cwd_root
    end

    set -l identify $USER@$hostname

    set -l stat
    if test $last_status -ne 0
        set stat (set_color $color_danger)"[$last_status]"(set_color $color_normal)
    end

    set -l cwd (set_color $color_cwd)(prompt_pwd)(set_color normal)

    set -l vcs
    set -l vcs_jj_or_git_branch (jj root >/dev/null 2>&1 || fish_git_prompt)
    if $vcs_jj_or_git_branch
        set vcs $vcs_jj_or_git_branch
    else
        set -l vcs_jj_revset '
            separate(" ",
            change_id.shortest(4),
            bookmarks,
            "|",
            concat(
                if(conflict, ""),
                if(divergent, ""),
                if(hidden, "H"),
                if(immutable, "I")
            ),
            raw_escape_sequence("\\x1b[1;32m]"))'

        set -l vcs_jj_template
        set vcs (jj log --revisions @ --no-graph --ignore-working-copy --color always --limit 1 --template)
    end
    and set -q vcs "[$vcs]"

    set -l sigil '$'
    if fish_is_root_user
        set sigil '#'
    end

    set -l cursor (set_color $color_normal)" "

    set -l taki_line '""""""""""""""""""""""""""""""""""""""""""""""""""""""""""|\\'

    printf "\n"
    string join ' ' -- $stat $identify $cwd $vcs $fish_git_prompt $taki_line
    string join '' -- $sigil $cursor

end
