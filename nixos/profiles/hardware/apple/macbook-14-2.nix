##: MacBook14,2 (A2681) -- <https://everymac.com/systems/apple/macbook-air/specs/macbook-air-m2-8-core-cpu-8-core-gpu-13-2022-specs.html>
{
  imports = [
    ./apple-silicon.nix
    ./macbook.nix
  ];

  # Machine-specific configurations can be appended to this initial
  # hardware-specific configuration.
  services.kmonad.keyboards."default".config = ''
    ${builtins.readFile ./default-layout.kbd}
  '';
}
