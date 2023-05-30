{
  config,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [awscli2];

  # aws-cli does not work well with xdg base directories
  # https://github.com/aws/aws-sdk/issues/30#issuecomment-532208981
  home.sessionVariables = {
    AWS_CONFIG_FILE = "${config.xdg.configHome}/aws/config";
    AWS_CLI_HISTORY_FILE = "${config.xdg.dataHome}/aws/history";
    AWS_CREDENTIALS_FILE = "${config.xdg.dataHome}/aws/credentials";
    AWS_SHARED_CREDENTIALS_FILE = "${config.xdg.dataHome}/aws/shared-credentials";
    AWS_WEB_IDENTITY_TOKEN_FILE = "${config.xdg.dataHome}/aws/token";
  };
}
