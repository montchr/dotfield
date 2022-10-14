{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    colima
    kubectl
    lima
    minikube
    qemu
  ];
}
