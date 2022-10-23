{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isLinux;
  inherit (config.dotfield.paths) storageBase;

  cfg = config.age;

  secretsDir = ../secrets;
  sshPath = "${storageBase}/etc/ssh";

  # nix-darwin does not support the `users.<name>.extraGroups` option, but
  # that's not a problem since we're only using darwin systems as a single
  # admin user. although the username may vary across systems, each "primary
  # user" will still be in the `admin` group.
  secretsGroup =
    if isLinux
    then "secrets"
    else "admin";

  mkEspansoMatchesSecret = name: {
    "espanso/${name}.yml" = {
      file = "${secretsDir}/espanso/${name}.yml.age";
      group = secretsGroup;
      path = "${cfg.secretsDir}/espanso/${name}.yml";
    };
  };
in {
  home-manager.sharedModules = [
    {
      # Allow running sops as a normal user without sudo.
      home.sessionVariables."SOPS_AGE_KEY_FILE" = "$XDG_CONFIG_HOME/sops/age/keys";
    }
  ];

  environment.systemPackages = with pkgs; [
    rage
    sops
  ];

  users.groups.secrets.members = ["root" "cdom" "seadoom" "xtallos"];

  # FIXME: avoid unnecessary mkmerge (it makes debugging harder)
  age.secrets = lib.mkMerge [
    (mkEspansoMatchesSecret "personal")
    # (mkEspansoMatchesSecret "work")
  ];

  sops.age.sshKeyPaths = ["${sshPath}/ssh_host_ed25519_key"];

  # This can be overridden per-host for localised secrets.
  sops.defaultSopsFile = lib.mkDefault ../secrets/global.secrets.yaml;
}
