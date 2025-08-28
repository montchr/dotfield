{ inputs, self, ... }:
{
  aspects.core.nixos =
    { pkgs, ... }:
    {
      imports = [
        inputs.home-manager.nixosModules.default
      ];

      home-manager = {
        # Prevent activation failures by specifying how to handle file
        # collisions.  Just back them up, don't freak out.
        backupFileExtension = "bak";
        extraSpecialArgs = self.lib.hm.specialArgs' pkgs.stdenv.hostPlatform.system;
        useGlobalPkgs = true;
        useUserPackages = true;
      };
    };

}
