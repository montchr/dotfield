{ config, ... }:
let
  features = config.dotfield.features;
in
{
  dotfield.features.workstation.home =
    { pkgs, ... }:
    {
      imports = [
        features."gpg/with-ssh".home
        features.password-store.home
      ];

      home.packages = [
        pkgs.monolith # <- bundle any web page into a single html file   => <https://github.com/Y2Z/monolith>
      ];
    };
}
