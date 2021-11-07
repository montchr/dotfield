{ config, lib, pkgs, ... }:

{
  my.hm.programs.direnv = {
    enable = true;
    # Prevent early loading of direnv integration for a faster shell startup.
    enableZshIntegration = false;
    nix-direnv.enable = true;
    nix-direnv.enableFlakes = true;
    stdlib = ''
      use_nvm() {
        local node_version="$1"

        nvm_sh="$NVM_DIR/nvm.sh"
        if [[ -e "$nvm_sh" ]]; then
          source "$nvm_sh"
          nvm use "$node_version"
        fi
      }
    '';
  };
}
