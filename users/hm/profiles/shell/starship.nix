{ config, lib, pkgs, ... }:

{
  programs.starship = {
    enable = true;
    enableZshIntegration = false;
    settings = {
      format = lib.concatStrings [
        "$username"
        "$hostname"
        "$shlvl"
        "$directory"
        "$git_branch"
        "$git_metrics"
        "$git_status"
        "$fill"
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

      aws = {
        symbol = " ";
      };


      character = {
        success_symbol = "❯";
        error_symbol = "[❱](bold red)";
        vicmd_symbol = "[❮](bold purple)";
      };


      battery = {
        full_symbol = "";
        charging_symbol = "";
        discharging_symbol = "";
      };


      conda = {
        symbol = " ";
      };


      directory = {
        style = "cyan";
        read_only = " ";
      };


      git_branch = {
        format = "[$symbol$branch]($style) ";
        symbol = " ";
        style = "bold dimmed white";
      };


      git_status = {
        format = "([$all_status$ahead_behind]($style) )";
        conflicted = " ";
        ahead = "$count ";
        behind = "$count ";
        diverged = " ";
        untracked = " ";
        stashed = " ";
        modified = " ";
        staged = " ";
        renamed = " ";
        deleted = " ";
        style = "bold bright-white";
      };

      memory_usage = {
        symbol = " ";
      };

      nix_shell = {
        format = "[$symbol$state]($style) ";
        symbol = " ";
        pure_msg = "λ";
        impure_msg = "⎔";
      };

      nodejs = {
        symbol = " ";
        version_format = "$major";
        format = "[$symbol($version )]($style)";
      };

      package = {
        symbol = " ";
      };

      php = {
        symbol = " ";
        format = "[$symbol($version )]($style)";
        version_format = "$major.$minor";
      };


      python = {
        symbol = " ";
      };


      ruby = {
        symbol = " ";
      };


      rust = {
        symbol = " ";
      };


      status = {
        disabled = false;
      };


    };
  };
}
