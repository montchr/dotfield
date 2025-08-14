{ moduleWithSystem, ... }:
{
  dotfield.features.jobwork.home = moduleWithSystem (
    perSystem@{ config, ... }:
    home@{ config, pkgs, ... }:
    {
      home.sessionVariables = {
        COMPOSER_HOME = "$XDG_STATE_HOME/composer";
      };

      home.packages = [
        perSystem.config.packages.wp-to-psr4

        pkgs.awscli2

        # https://towardsthecloud.com/set-up-aws-cli-aws-sso
        # https://www.granted.dev/
        # https://github.com/common-fate/granted
        # the aws cli is so terrible that it needs a helper just to log in with sso
        # (and aws really pushes sso hard, oops)
        # and unfortunately, the static access keys reset every 10 minutes or so,
        # making even the simple escape hatch unbearable. so much time wasted
        # waiting for an rclone s3 command to complete only to see that the tokens expired.
        pkgs.granted

        pkgs.phpactor

        pkgs.wp-cli

        # Provides DAP connection to Xdebug for editor support
        pkgs.vscode-extensions.xdebug.php-debug

        # Requires further setup: <https://wiki.nixos.org/wiki/Jetbrains_Tools#JetBrains_Toolbox>
        pkgs.jetbrains-toolbox

      ];
    }
  );
}
