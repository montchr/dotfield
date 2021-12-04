{ config, lib, pkgs, ... }:

{
  age.secrets.bortHole.file = "${config.dotfield.dir}/secrets/networks/bortHole.age";
}
