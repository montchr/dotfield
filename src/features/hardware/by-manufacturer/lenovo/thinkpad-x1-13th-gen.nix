{ inputs, ... }:
{
  aspects.hardware__lenovo__thinkpad-x1-13th-gen = {
    requires = [
      "hardware__fingerprint-scanner"
      "laptop"
    ];
    nixos = {
      imports = [
        inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-13th-gen
      ];
    };
  };
}
