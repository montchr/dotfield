{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.starship = {
    enable = true;
    settings = {
      add_newline = true;

      format = lib.concatStrings [
        "$username"
        "$hostname"
        "$shlvl"
        "$localip"
        "$container"
        "$docker_context"
        "$directory"
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

      character = {
        success_symbol = "❯";
        error_symbol = "[](bold red)";
        vicmd_symbol = "[❮](bold purple)";
      };

      battery = {
        full_symbol = "";
        charging_symbol = "";
        discharging_symbol = "";
      };

      directory = {
        style = "cyan";
        read_only = " ";
      };

      nix_shell = {
        symbol = " ";
        pure_msg = "λ ";
        impure_msg = "⎔ ";
      };

      nodejs = {
        symbol = " ";
        version_format = "$major";
      };

      php = {
        symbol = " ";
        version_format = "$major.$minor";
      };

      aws.symbol = " ";
      conda.symbol = " ";
      docker_context.symbol = " ";
      fill.symbol = " ";
      memory_usage.symbol = " ";
      package.symbol = " ";
      python.symbol = " ";
      ruby.symbol = " ";
      rust.symbol = " ";

      shlvl.disabled = false;
      status.disabled = false;
    };
  };
}
