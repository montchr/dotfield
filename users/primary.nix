{
  username,
  hashedPassword ? "$6$yq7jJybfGyx19QqK$mr1dfKu1fChKkYDUZvQnlcKCmAYywIvWZXw3uT9EjQ/Xi85SGqkPDcsrrQ.7WEYM6InqDPqGZrTGfvoFpuONi1",
}: {
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;

  secretsDir = ../secrets;

  sessionUser = builtins.getEnv "USER";
  name =
    if builtins.elem sessionUser ["" "root"]
    then username
    else sessionUser;

  userCfg = config.users.users.${name};
  sshHome = "${userCfg.home}/.ssh";
in {
  users.users.${name} = lib.mkMerge [
    {
      shell = pkgs.zsh;
      home =
        if isDarwin
        then "/Users/${name}"
        else "/home/${name}";
    }
    (lib.optionalAttrs isLinux {
      extraGroups = [
        pkgs.lib.our.dotfield.group
        "wheel"
      ];
      hashedPassword = lib.mkDefault hashedPassword;
      isNormalUser = true;
    })
  ];

  age.identityPaths = lib.mkBefore [
    "${sshHome}/id_ed25519"
  ];

  age.secrets."aws/aws-cdom-default.pem" = {
    file = "${secretsDir}/aws/aws-cdom-default.pem.age";
    path = "${sshHome}/.ssh/aws-cdom-default.pem";
    owner = name;
  };
}
