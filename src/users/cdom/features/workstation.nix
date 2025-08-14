{
  dotfield.features.workstation.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.monolith # <- bundle any web page into a single html file   => <https://github.com/Y2Z/monolith>
      ];
    };
}
