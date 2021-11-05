# -*- mode: nix -*-

{ config, pkgs, lib, home-manager, options, ... }:
let
  inherit (lib) types;
  inherit (pkgs.lib.our) mkOpt mkOpt' mkBoolOpt;
in
{
  options = with types; {

    my = {
      name = mkOpt str "Chris Montgomery";
      timezone = mkOpt str "America/New_York";
      username = mkOpt str "montchr";
      website = mkOpt str "https://github.com/montchr";
      githubUsername = mkOpt str "montchr";
      email = mkOpt str "chris@cdom.io";
      terminal = mkOpt str "kitty";
      nix_managed = mkOpt str
        "DO NOT EDIT! - managed by Nix - see source inside ${config.dotfield.dir}";
      user = lib.mkOption { type = options.users.users.type.functor.wrapped; };

      keys = {
        pgp = mkOpt str "0x135EEDD0F71934F3";
        ssh = mkOpt (listOf string) [
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC3tWcOwvNOfHXX3YvtLmJRigxATUh++bWRCAM07uy3mbNvEteT5bF/7nixO44gep0Hv24jaqLeGjCaTxFXrmt1NGgvmAXcsoS4I3+N2xfiFZPIKoiF0EONDsInjm4h5eNoPPE4Rd9xLju4S4tXaXDcL37PunQZJ+aR6CRVf/geM+H4y70cvYHV6uakMAfuv/0+AEMLwlSIN7OpDN8B+JGI4rQhBsekRkkkcZlPYO4vT63aTvLCYFxJ/fR45oMKW57lvZUrbRMHbKRkOfyhBF3qbYR/9aMEUd7gjYBfLJ1hQaHlp2aV49m53WFBjmjqjFcxDPxS/HMk/Hazowkw0G6iNzSNHnO5wI/BxIEahavYvd4VOQXpaWs/G58t8kdQol8WFufLjAReP0j16TqcWEHwy1ktMcrpYfDlLSlNcuaUeXJNIyvD3WmfRDXBnxlBenFIqe9lnK8RUVCcxM+lEEJbMWs1ZuWmgXjbt3UkFhSKSv2Adlm2/OfBBCyO46hVmhLfkwzB69aXYqUjPthlvtCDuLxrmT+DZeWsucUKPp2L9PXS6LpbpnIWCqmnGIPLjHBX2X3EOKwrtLAGN5wv7zLv88qHOD0MET2KVZkfTLg04FkcNowNwAlQ8xBBjpt6xEWNFMH532ZRO1CT0VTUNB7nEW2JET1SULsRT/bTUbKQHQ== chris@cdom.io"
        ];
      };

      hm = lib.mkOption {
        description = "Primary user's home-manager configuration";
        type = types.attrs;
        default = { };
      };

      xdg = let inherit (config.my.user) home; in
        {
          bin = mkOpt path "${home}/.local/bin";
          cache = mkOpt path "${home}/.cache";
          config = mkOpt path "${home}/.config";
          data = mkOpt path "${home}/.local/share";
          lib = mkOpt path "${home}/.local/lib";
        };

      env = lib.mkOption {
        type = attrsOf (oneOf [ str path (listOf (either str path)) ]);
        apply = lib.mapAttrs (n: v:
          if lib.isList v then
            lib.concatMapStringsSep ":" (x: toString x) v
          else
            (toString v));
        default = { };
        description = "Environment variables.";
      };
    };
  };
}
