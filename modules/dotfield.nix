{ config, lib, pkgs, ... }:

let
  inherit (pkgs.lib.our) mkOpt;
  dir = (toString ../.);
in

{
  options = {
    dotfield = with lib.types; {
      dir = mkOpt path dir;

      binDir = mkOpt path "${dir}/bin";
      configDir = mkOpt path "${dir}/config";
      libDir = mkOpt path "${dir}/lib";
      modulesDir = mkOpt path "${dir}/modules";
      profilesDir = mkOpt path "${dir}/profiles";
      vendorDir = mkOpt path "${dir}/vendor";

      # FIXME: This points to an arbitrary location which may vary per system.
      # Instead, it should be determined programmatically based on the flake's
      # actual location. Note, however, that its currently only useful for
      # out-of-store symlinks, which are generally discouraged as they do not
      # adhere to the Nix principle of immutable configuration.
      path = mkOpt path "${config.my.user.home}/.config/dotfield";
    };
  };
}
