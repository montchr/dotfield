flake@{ ... }:
let
  inherit (builtins) map;
in
{
  dotfield.users.cdom.aspects.workstation.home =
    { pkgs, ... }:
    {
      imports =
        (with flake.config.dotfield.aspects; [
          development
          git__with-gpg-signing
          jujutsu__with-gpg-signing
          jujutsu__with-sign-on-push
        ])
        ++ (with flake.config.dotfield.users.cdom.aspects; [
          ai
          development
          mail
          music-production
        ])
        |> map (v: v.home);

      services.gpg-agent.enableSshSupport = true;
      services.gpg-agent.enableExtraSocket = true;

    };
}
