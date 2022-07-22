{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isLinux system;

  cfg = config.age;
  secretsDir = ../secrets;

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
  environment.variables."SOPS_AGE_KEY_FILE" = "$XDG_CONFIG_HOME/sops/age/keys";

  environment.systemPackages = with pkgs; [
    agenix
    rage
    sops
  ];

  users.groups.secrets.members = ["root" "cdom" "seadoom" "xtallos"];

  age.secrets = lib.mkMerge [
    (mkEspansoMatchesSecret "personal")
    # (mkEspansoMatchesSecret "work")
  ];

  sops.defaultSopsFile = ../secrets/global.secrets.yaml;
  sops.age.sshKeyPaths = ["/etc/ssh/ssh_host_ed25519_key"];
  sops.age.keyFile = "/var/lib/sops-nix/key";
}
