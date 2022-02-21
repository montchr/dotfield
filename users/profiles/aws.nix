{ config, lib, pkgs, ... }:

{
  my.user.packages = with pkgs; [ awscli2 ];

  # aws-cli does not work well with xdg base directories
  # https://github.com/aws/aws-sdk/issues/30#issuecomment-532208981
  my.env = {
    AWS_CONFIG_FILE = "$XDG_CONFIG_HOME/aws/config";
    AWS_CLI_HISTORY_FILE = "$XDG_DATA_HOME/aws/history";
    AWS_CREDENTIALS_FILE = "$XDG_DATA_HOME/aws/credentials";
    AWS_SHARED_CREDENTIALS_FILE = "$XDG_DATA_HOME/aws/shared-credentials";
    AWS_WEB_IDENTITY_TOKEN_FILE = "$XDG_DATA_HOME/aws/token";
  };
}
