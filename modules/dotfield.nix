{ config, lib, pkgs, ... }:

let
  inherit (pkgs.lib.our) mkOpt mkOpt' mkBoolOpt;

  t = with lib.types; either str path;
in

{
  options = {
    dotfield = rec {
      configDir = mkOpt t "${config.dotfield.dir}/config";
      dir = mkOpt t (toString ../.);
      # FIXME: This points to an arbitrary location which may vary per system.
      # Instead, it should be determined programmatically based on the flake's
      # actual location. Note, however, that its currently only useful for
      # out-of-store symlinks, which are generally discouraged as they do not
      # adhere to the Nix principle of immutable configuration.
      path = mkOpt t "${config.my.user.home}/.config/dotfield";
      binDir = mkOpt t "${config.dotfield.dir}/bin";
      libDir = mkOpt t "${config.dotfield.dir}/lib";
      modulesDir = mkOpt t "${config.dotfield.dir}/modules";
      vendorDir = mkOpt t "${config.dotfield.dir}/vendor";
    };
  };
}
