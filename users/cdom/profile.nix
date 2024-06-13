{
  imports = [ ./profiles/rclone.nix ];

  programs.git.delta.enable = true;
  programs.nnn.enable = true;

  programs.zsh.enable = true;
  programs.zsh.grml.enable = true;
}
