{ ... }: {
  programs.git = {
    userName = "Chris Montgomery";
    userEmail = "chris@alley.co";
    signing = {
      key = "chris@alley.co";
      signByDefault = true;
    };
    extraConfig = {
      github.user = "montchr";
    };
  };
}
