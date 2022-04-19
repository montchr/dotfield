{...}: let
  inherit (default.inputs.nixpkgs-stable) lib;

  default = (import ../.).defaultNix;
  configs = default.darwinConfigurations;
  host = configs.${hostname} or configs.macOS;

  # TODO: /etc/hostname does not seem to exist on Darwin hosts as of 2022. The
  # file is un-writable (even with sudo) because the admin user is not in the
  # `wheel` group, which the nix installer sets as the owner group of the system
  # root directory. However, it doesn't appear to be (currently) possible to add
  # an admin user to `wheel` because nix-darwin has, for some reason, commented
  # out the declaration of `users.users.<username>.extraGroups`...
  #
  # https://github.com/LnL7/nix-darwin/blob/4fdbb8168f61d31d3f90bb0d07f48de709c4fe79/modules/users/user.nix#L42-L46
  #
  # TODO: note which Darwin/macOS release changed the existence of /etc/hostname
  # ...or is this missing file and permissions issue entirely a mistake of my
  # own creation? i'm not sure...
  hostname = builtins.getEnv "HOSTNAME";
in
  host
