{
  users.cdom.aspects.core.home =
    { pkgs, ... }:
    let
      shellAliases = import ./__aliases.nix { inherit pkgs; };
    in
    {
      home = { inherit shellAliases; };

      home.packages = [
        pkgs.chawan # <- tui web browser

      ];
    };

  users.cdom.aspects.workstation.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.monolith # <- bundle any web page into a single html file # => <https://github.com/Y2Z/monolith>

        (pkgs.writeShellApplication {
          name = "color-panic";
          runtimeInputs = [ pkgs.colorpanes ];
          text = ''
            colorpanes --captions --height 38 --width 24
          '';
        })

      ];
    };
}
