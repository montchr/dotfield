{
  dotfield.modules."gpg/with-ssh".home =
    { config, pkgs, ... }:
    {
      services.gpg-agent = {
        enableSshSupport = true;
        enableExtraSocket = true;
      };
    };
}
