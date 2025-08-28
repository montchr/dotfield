{ config, self, ... }:
let
  inherit (config.meta.users.cdom) whoami;
  profiles = self.outPath + "/home/profiles";
in
{
  aspects.core.home =
    { pkgs, ... }:
    let
      toColorSchemePath = self.lib.theme.toColorSchemePath pkgs;
    in
    {
      imports = [
        ./features/starship

        (profiles + "/atuin.nix")
        (profiles + "/development/nix-tools.nix")
        (profiles + "/shells/fish/default.nix")
        (profiles + "/neovim/default.nix")
        (profiles + "/nnn.nix")
        (profiles + "/rclone.nix")
        (profiles + "/ssh.nix")
        (profiles + "/yazi.nix")
        (profiles + "/zellij.nix")
        (profiles + "/zoxide.nix")
      ];

      programs.ghostty.settings.command = "fish";

      programs.git.signing.key = whoami.pgp.id;
      services.gpg-agent = {
        enableSshSupport = true;
        enableExtraSocket = true;
      };

      stylix.base16Scheme = toColorSchemePath "catppuccin-mocha";
    };
}
