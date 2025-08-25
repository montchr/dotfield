flake:
let
  inherit (flake.config.dotfield) meta;
  inherit (builtins)
    attrNames
    foldl'
    filter
    mapAttrs
    ;
  hostAspects = flake.config.dotfield.hosts.nixos.tuuvok.aspects;

  /**
    FIXME: this will not work -- the input aspects will already be a
    list in the single use case, which we cannot work with...?

    Given an attibute set of aspects, return a list of those aspects in
    addition to the corresponding user aspects, if any.
  */
  # decorateAspectsForUser =
  #   username: globalAspects:
  #   let
  #     userAspects = flake.config.dotfield.users.${username}.aspects;
  #   in
  #   attrNames globalAspects
  #   |> foldl' (acc: aspect: acc ++ [ (userAspects.${aspect} or false) ]) globalAspects
  #   |> filter (v: v != false);
in
{
  dotfield.hosts.nixos.tuuvok = {
    users.cdom = {
      aspects = hostAspects
      # (decorateAspectsForUser "cdom" hostAspects)
      ;

      home = {
        # FIXME: this no longer works!  causes error
        # programs.firefox.profiles.work.isDefault = true;
        # programs.firefox.profiles.home.isDefault = false;

        wayland.windowManager.sway.config.startup = [
          { command = "teams-for-linux"; }
        ];

        home.stateVersion = "23.05";
      };
    };

    nixos =
      {
        config,
        pkgs,
        ...
      }:
      let
        username = "cdom";
      in
      {
        sops.secrets."users/${username}/hashed-password".neededForUsers = true;

        users.users.${username} = {
          uid = 1000;
          isNormalUser = true;
          hashedPasswordFile = config.sops.secrets."users/${username}/hashed-password".path;
          openssh.authorizedKeys.keys = meta.users.cdom.keys.ssh;
          shell = pkgs.bashInteractive;
        };
      };
  };
}
