# <https://discourse.nixos.org/t/nvd-simple-nix-nixos-version-diff-tool/12397>
# <https://github.com/Mic92/dotfiles/blob/c6cad4e57016945c4816c8ec6f0a94daaa0c3203/nixos/modules/upgrade-diff.nix>
# <https://github.com/NixOS/nixpkgs/pull/208902>
{ pkgs, ... }:
{
  # NOTE: All linked examples use /run/current-system, probably to preserve
  #       existing NixOS behavior since profiles still require opt-in as an
  #       experimental feature. However, nix-darwin AFAIK does not have such a
  #       path. The system profile, as used below, points to the same derivation
  #       as /run/current-system, so no platform check is necessary.
  # FIXME: does not seem to do anything, either during activation or in an interactive shell session
  system.activationScripts.diff = ''
    ${pkgs.nix}/bin/nix store diff-closures \
      /nix/var/nix/profiles/system "$systemConfig" || true
  '';
}
