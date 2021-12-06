{ config, lib, pkgs, ... }:

let
  inherit (config.dotfield) secretsDir;
in

{
  age.secrets."wireless.env".file = "${secretsDir}/wireless.env.age";
}
