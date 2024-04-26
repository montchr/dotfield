# <https://github.com/NixOS/nixpkgs/pull/208902>
{ pkgs, ... }:
{
  # FIXME: no effect
  system.activationScripts.diff = ''
    if [[ -e /run/current-system ]]; then
      ${pkgs.nix}/bin/nix --extra-experimental-features 'nix-command' store diff-closures /run/current-system "$systemConfig" || true
    fi
  '';
}
