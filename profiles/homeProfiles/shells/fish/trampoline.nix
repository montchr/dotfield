{config, ...}: let
  cfg = config.programs.fish;
in {
  programs.bash.trampoline.enable = true;
  programs.bash.trampoline.shell.package = cfg.package;
}
