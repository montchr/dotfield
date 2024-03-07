{ flake, ... }:
let
  l = flake.inputs.nixpkgs.lib // builtins;
in
{
  imports = [ ./extra-symbols.nix ];
  programs.starship = {
    enable = true;
    #: TODO: 
    settings = {
      ##: --- global ("prompt") ---

      add_newline = true;
      format = l.concatStrings [
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
      directory = {
        style = "cyan";
        read_only = " ";
      };
      fill.symbol = " ";
      status = {
        symbol = " "; # nf-cod-error
        success_symbol = "";
        not_executable_symbol = " "; # nf-mdi-minus_circle_outline
        not_found_symbol = " "; # nf-cod-question
        sigint_symbol = "ﰸ "; # nf-mdi-cancel
        signal_symbol = ""; # nf-fa-bolt
        map_symbol = true;
        disabled = false;
      };

      ##: --- vcs ---

      git_branch.symbol = " ";
      git_status = {
        conflicted = " ";
        ahead = " ";
        behind = " ";
        diverged = " "; # alt: 
        up_to_date = " ";
        untracked = " "; # alt: 
        stashed = " "; # alt: 
        modified = " ";
        staged = " ";
        renamed = " ";
        deleted = " ";
      };

      ##: --- languages ---

      golang.symbol = " ";
      haskell.symbol = " ";
      lua.symbol = " ";
      nodejs = {
        symbol = " ";
        version_format = "$major";
      };
      php = {
        symbol = " ";
        version_format = "v\${raw}";
        # version_format = "$major.$minor";
      };
      python.symbol = " ";
      ruby.symbol = " ";
      rust.symbol = " ";

      ##: --- system resources ---

      battery = {
        full_symbol = "";
        charging_symbol = "";
        discharging_symbol = "";
      };
      memory_usage.symbol = " ";

      ##: --- environments ---

      docker_context.symbol = " ";
      hostname = {
        ssh_only = true;
        ssh_symbol = " "; # alt: 
      };
      localip.disabled = false;
      nix_shell = {
        symbol = " ";
        pure_msg = "ﬦ";
        impure_msg = "ﬦ";
      };
      package.symbol = " ";
      shlvl.disabled = false;
      terraform.symbol = " ";
    };
  };
}
