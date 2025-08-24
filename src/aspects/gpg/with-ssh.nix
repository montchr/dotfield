{
  dotfield.aspects.gpg__with-ssh.home =
    { config, pkgs, ... }:
    {
      services.gpg-agent = {
        enableSshSupport = true;
        enableExtraSocket = true;
      };
    };
}
