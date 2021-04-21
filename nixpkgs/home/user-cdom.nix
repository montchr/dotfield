{ ... }: {
  programs.git = {
    userName = "Chris Montgomery";
    userEmail = "chris@cdom.io";
    signing = {
      key = "chris@cdom.io";
      signByDefault = true;
    };
    extraConfig = {
      github.user = "montchr";
    };
  };
}
