for p in /run/current-system/sw/bin
    if not contains $p $fish_user_paths
        set -U fish_user_paths $p $fish_user_paths
    end
end

set -U fish_user_paths /run/wrappers/bin $fish_user_paths

function fish_title
  echo "$PWD | $_" | sed "s|$HOME|~|g"
end
