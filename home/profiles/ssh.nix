{
  programs.ssh = {
    enable = true;
    forwardAgent = false;

    matchBlocks."github.com" = {
      # inherit identityFile;
      # identitiesOnly = true;
      user = "git";
    };
  };
}
