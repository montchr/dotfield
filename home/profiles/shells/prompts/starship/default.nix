{ lib, ... }:
{
  imports = [ ./extra-symbols.nix ];
  programs.starship = {
    enable = true;
    #: TODO: 
    settings = {
      ##: --- global ("prompt") ---

      add_newline = true;
      format = lib.concatStrings [
        "$username"
        "$hostname"
        "$shlvl"
        "$localip"
        "$container"
        "$docker_context"
        "$directory"
        # HACK: jujutsu support
        # unfortunately difficult to override in the jj module
        "\${custom.jj}\${custom.jj_git_branch_fallback}"
        "$git_branch"
        "$git_commit"
        "$git_state"
        "$git_metrics"
        "$git_status"
        "$fill"
        "$deno"
        "$nodejs"
        "$php"
        "$python"
        "$ruby"
        "$terraform"
        "$vagrant"
        "$nix_shell"
        "$cmd_duration"
        "$line_break"
        "$jobs"
        "$time"
        "$status"
        "$shell"
        "$character"
      ];

      directory = {
        # style = "cyan";
        read_only = " ";
      };
      #   fill.symbol = " ";

      haskell.symbol = " ";
      nodejs = {
        symbol = " ";
        version_format = "$major";
      };
      #   php = {
      #     symbol = " ";
      #     version_format = "v\${raw}";
      #     # version_format = "$major.$minor";
      #   };
      # python.symbol = " ";
      ruby.symbol = " ";
      rust.symbol = " ";

      #   ##: --- system resources ---

      #   battery = {
      #     full_symbol = "";
      #     charging_symbol = "";
      #     discharging_symbol = "";
      #   };
      #   memory_usage.symbol = " ";

      #   ##: --- environments ---

      docker_context.symbol = " ";
      hostname = {
        ssh_only = true;
      };

      nix_shell = {
        symbol = " ";
      };
      shlvl.disabled = false;
    };
  };
}
