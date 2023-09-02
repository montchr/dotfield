{config, ...}: let
  cfg = config.programs.nushell;
in {
  programs.bash.trampoline.enable = true;
  programs.bash.trampoline.shell.package = cfg.package;
}
