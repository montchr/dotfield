{
  inputs,
  cell,
}: let
  inherit (inputs) apparat nixpkgs;
  inherit (apparat.lib.devshell) pkg;
  cmd = pkg "secrets";
in {
  default = _: {
    commands = [
      (cmd nixpkgs.age-plugin-yubikey)
      (cmd nixpkgs.rage)
      (cmd nixpkgs.sops)
      (cmd nixpkgs.ssh-to-age)
      (cmd nixpkgs.yubikey-manager)
    ];
    nixago = [
      (cell.cfg.sops {})
    ];
    packages = [];
  };
}
