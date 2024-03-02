{pkgs, ...}: {
  homebrew.casks = ["bitwarden"];
  environment.systemPackages = [pkgs.bitwarden-cli];
}
