{ inputs, ... }:
{
  aspects.development__kleinweb = {
    requires = [
      "development"
      "development__php"
    ];

    nixos = {
      imports = [
        inputs.beams.modules.nixos.default
      ];
    };
  };
}
