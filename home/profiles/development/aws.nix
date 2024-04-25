{ config, pkgs, ... }:
{
  home.packages = [ pkgs.awscli2 ];

  # aws-cli does not work well with xdg base directories
  # https://github.com/aws/aws-sdk/issues/30#issuecomment-532208981
  # TODO: this should probably just be removed, because 1) some directories
  # cannot be customized with environment variables (e.g. cache) and 2) AWS will
  # likely do nothing to improve the situation and 3) the only benefit here is a
  # less-cluttered home directory + predictability
  home.sessionVariables = {
    AWS_CONFIG_FILE = "${config.xdg.configHome}/aws/config";
    AWS_CLI_HISTORY_FILE = "${config.xdg.dataHome}/aws/history";
    AWS_CREDENTIALS_FILE = "${config.xdg.dataHome}/aws/credentials";
    AWS_SHARED_CREDENTIALS_FILE = "${config.xdg.dataHome}/aws/shared-credentials";
    AWS_WEB_IDENTITY_TOKEN_FILE = "${config.xdg.dataHome}/aws/token";
  };
}
