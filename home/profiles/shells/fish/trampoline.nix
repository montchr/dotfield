{ config, ... }:
let
  cfg = config.programs.fish;
in
{
  imports = [ ./default.nix ];
  programs.bash.trampoline.enable = true;
  programs.bash.trampoline.shell.package = cfg.package;
}
