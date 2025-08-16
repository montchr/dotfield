{ config, ... }:
let
  features = config.dotfield.users.cdom.features;
in
{
  dotfield.users.cdom.features.workstation.home =
    { pkgs, ... }:
    {
      imports = [
        features.gpg__with-ssh.home
        features.password-store.home
      ];

      home.packages = [
        pkgs.monolith # <- bundle any web page into a single html file   => <https://github.com/Y2Z/monolith>
      ];
    };
}
