{ config, lib, ... }:
let
  # inherit (builtins)
  #   concatStringsSep
  #   isBool
  #   isList
  #   replaceStrings
  #   toString
  #   ;
  # inherit (lib)
  #   boolToString
  #   mapAttrs'
  #   mkValueStringDefault
  #   nameValuePair
  #   toUpper
  #   ;

  # settings = {
  #   inherit (config.nix.settings) substituters trusted-public-keys;
  #   keep-builds-running = true;
  # };
  # mkVarName = name: ("NIXBUILDNET_" + (toUpper (replaceStrings [ "-" ] [ "_" ] name)));
  # mkVarValue = k: v: ["${mkVarName k}=${mkValueStringDefault {} v}"];
  # value:
  # if isBool value then
  #   boolToString value
  # else if isList value then
  #   (concatStringsSep " " value)
  # else
  #   toString value;
  # mkList = k: v: concatMap (mkVarValue k);
  # mkVars = name: value: nameValuePair (mkVarName name) (mkVarValue value);
  # settings' = mapAttrs' mkVars settings;
  # env = ;

in
{
  programs.ssh = {
    extraConfig = ''
      Host eu.nixbuild.net
        PubkeyAcceptedKeyTypes ssh-ed25519
        ServerAliveInterval 60
        IPQoS throughput
        # NOTE: Each host must generate this key!
        IdentityFile /etc/ssh/id_ed25519_seadome_nixbuild_net
    '';

    knownHosts = {
      nixbuild = {
        hostNames = [ "eu.nixbuild.net" ];
        publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPIQCZc54poJ8vqawd8TraNryQeJnvH1eLpIDgbiqymM";
      };
    };
  };

  nix = {
    distributedBuilds = true;
    buildMachines = [
      {
        hostName = "eu.nixbuild.net";
        system = "x86_64-linux";
        maxJobs = 100;
        supportedFeatures = [
          "benchmark"
          "big-parallel"
        ];
      }
      {
        hostName = "eu.nixbuild.net";
        system = "aarch64-linux";
        maxJobs = 100;
        supportedFeatures = [
          "benchmark"
          "big-parallel"
        ];
      }
    ];
  };
}
