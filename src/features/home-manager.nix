{
  aspects.core.nixos =
    { pkgs, ... }:
    {
      home-manager = {
        # Prevent activation failures by specifying how to handle file
        # collisions.  Just back them up, don't freak out.
        backupFileExtension = "bak";
        useGlobalPkgs = true;
        useUserPackages = true;
      };
    };

}
