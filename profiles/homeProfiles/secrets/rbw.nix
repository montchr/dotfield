{
  flake,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  l = flake.inputs.nixpkgs.lib // builtins;
in {
  home.packages = l.optional isDarwin pkgs.pinentry_mac;
  programs.rbw = {
    enable = true;
    # FIXME: allow multiple accounts somehow...
    # settings = {
    #   email = l.mkDefault email;
    #   pinentry = l.mkIf isDarwin pkgs.pinentry_mac;
    # };
  };
}
