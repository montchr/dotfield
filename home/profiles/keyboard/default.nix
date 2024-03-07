{ lib, pkgs, ... }: lib.mkMerge [ { home.packages = with pkgs; [ ]; } ]
