# cf. https://github.com/jj-vcs/jj/wiki/Starship
{
  lib,
  config,
  flake,
  ...
}:
lib.mkIf config.programs.starship.enable {
  home.packages = [ flake.perSystem.packages.starship-jj ];

  programs.starship.settings = {
    # FIXME: nope, infinite recursion, gotta do it in the starship
    # module where prompt is set initially
    # format =
    #   lib.replaceString "$git_branch" "\${custom.jj}\${custom.jj_git_branch_fallback}"
    #     config.programs.starship.settings.format;

    git_branch.disabled = true;
    git_commit.disabled = true;
    git_metrics.disabled = true;
    git_state.disabled = true;
    git_status.disabled = true;

    custom.jj = {
      detect_folders = [ ".jj" ];
      ignore_timeout = true;
      symbol = "c󰝴";
      command = ''
        jj log --revisions @ --no-graph --ignore-working-copy --color always --limit 1 --template '
          separate(" ",
            change_id.shortest(4),
            bookmarks,
            "|",
            concat(
              if(conflict, ""),
              if(divergent, ""),
              if(hidden, "󰘓"),
              if(immutable, ""),
            ),
            raw_escape_sequence("\x1b[1;32m") ++ if(empty, "(empty)"),
            raw_escape_sequence("\x1b[1;32m") ++ coalesce(
              truncate_end(72, description.first_line(), "…"),
              "(no description set)",
            ) ++ raw_escape_sequence("\x1b[0m"),
          )
        '
      '';

      # for starship-jj tool (disabled because bad handling of unicode characters):
      # command = "prompt";
      # format = "$output";
      # shell = [
      #   "starship-jj"
      #   "--ignore-working-copy"
      #   "starship"
      # ];
      # use_stdin = false;
    };

    custom.git_branch_fallback = {
      when = true;
      command = "jj root >/dev/null 2>&1 || starship module git_branch";
      description = "Display $git_branch if we're not in a jj repo";
    };
  };
}
