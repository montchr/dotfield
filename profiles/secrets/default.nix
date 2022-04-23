{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isLinux system;

  agenix-cli = inputs.agenix-cli.packages.${system}.agenix-cli;
  cfg = config.age;
  secretsDir = ../../secrets;

  # nix-darwin does not support the `users.<name>.extraGroups` option, but
  # that's not a problem since we're only using darwin systems as a single
  # admin user. although the username may vary across systems, each "primary
  # user" will still be in the `admin` group.
  dotfieldGroup = if isLinux
    then pkgs.lib.our.dotfield.group
    else "admin";

  mkEspansoMatchesSecret = name: {
    "espanso-${name}.yml" = {
      file = "${secretsDir}/espanso/${name}.yml.age";
      group = dotfieldGroup;
      path = "${cfg.secretsDir}/espanso/${name}.yml";
    };
  };
in {
  environment.systemPackages = with pkgs; [
    agenix-cli
    rage
  ];

  age.secrets = lib.mkMerge [
    {
      "aws-cdom-default.pem" = {
        file = "${secretsDir}/aws/aws-cdom-default.pem.age";
        group = dotfieldGroup;
      };

      "wireless.env".file = "${secretsDir}/wireless.env.age";
    }
    (mkEspansoMatchesSecret "personal")
    (mkEspansoMatchesSecret "work")
  ];

}
