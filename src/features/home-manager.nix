{ inputs, self, ... }:
{
  aspects.core.nixos =
    { pkgs, ... }:
    {
      home-manager = {
        # Prevent activation failures by specifying how to handle file
        # collisions.  Just back them up, don't freak out.
        backupFileExtension = "bak";
        # FIXME: no more of this nonsense
        extraSpecialArgs = self.lib.hm.specialArgs' pkgs.stdenv.hostPlatform.system;
        useGlobalPkgs = true;
        useUserPackages = true;
      };
    };

}
