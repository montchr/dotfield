{ sharedProfiles, ... }:
{
  imports = [ sharedProfiles.core.builders.nixbuild-net ];

  environment.etc."ssh/ssh_config.d/nixbuild-net".text = ''
    Host eu.nixbuild.net
      PubkeyAcceptedKeyTypes ssh-ed25519
      IdentityFile /etc/ssh/ssh_nixbuildnet_ed25519_key
  '';
}
