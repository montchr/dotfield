{ pkgs, ... }:
{
  homebrew.casks = [ "docker" ];
  environment.systemPackages = with pkgs; [
    colima
    kubectl
    lima
    minikube
    qemu
  ];
}
