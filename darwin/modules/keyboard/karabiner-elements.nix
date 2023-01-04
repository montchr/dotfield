{
  inputs,
  config,
  ...
}: let
  l = inputs.nixpkgs.lib // builtins;
  o = l.options;
  t = l.types;
  cfg = config.keyboard.karabiner-elements;
in {
  options.keyboard.karabiner-elements = {
    enable = o.mkEnableOption "Karabiner-Elements";
    installCask = o.mkOption {
      type = t.bool;
      default = true;
      description = ''
        Whether to install Karabiner-Elements as a Homebrew Cask.
      '';
    };
  };
  config = l.mkIf cfg.enable {
    assertions = [
      {
        assertion = !config.services.karabiner-elements.enable or true;
        message = ''
          The `keyboard.karabiner-elements` module is incompatible with
          nix-darwin's `services.karabiner-elements` module.
          Please choose one or the other.
        '';
      }
    ];
    services.karabiner-elements.enable = l.mkForce false;
    homebrew.casks = ["karabiner-elements"];
  };
}
