{ config, lib, pkgs, inputs, ... }:

let
  agenix-cli = inputs.agenix-cli.defaultPackage.${pkgs.stdenv.system};
in

{
  environment.systemPackages = with pkgs; [
    agenix-cli
    rage
  ];
}
