{ config, ... }:
let
  aspects = config.dotfield.users.cdom.aspects;
in
{
  dotfield.users.cdom.aspects.workstation.home =
    { pkgs, ... }:
    {
      imports = [
        aspects.gpg__with-ssh.home
        aspects.password-store.home
      ];

      home.packages = [
        pkgs.monolith # <- bundle any web page into a single html file   => <https://github.com/Y2Z/monolith>
      ];
    };
}
