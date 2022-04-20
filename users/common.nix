username: {
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;

  sessionUser = builtins.getEnv "USER";
  name =
    if builtins.elem sessionUser ["" "root"]
    then username
    else sessionUser;
in {
  users.users.${name} = lib.mkMerge [
    {
      shell = pkgs.zsh;
      home =
        if isDarwin
        then "/Users/${name}"
        else "/home/${name}";
      # TODO: this SHOULD exist in nix-darwin, but it doesn't yet
      # extraGroups = ["wheel"];
    }
    (lib.optionalAttrs isLinux {
      extraGroups = ["wheel"];
      isNormalUser = true;
      # FIXME: use different passwords! but be careful -- hashed passwords can fail (it's happened to me)
      # https://github.com/NixOS/nixpkgs/issues/136104
      hashedPassword = "$6$yq7jJybfGyx19QqK$mr1dfKu1fChKkYDUZvQnlcKCmAYywIvWZXw3uT9EjQ/Xi85SGqkPDcsrrQ.7WEYM6InqDPqGZrTGfvoFpuONi1";
    })
  ];
}
