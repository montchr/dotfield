{
  programs.git.includes =
    let
      contents = {
        # TODO: add this to an identity config -- dotfield's whoami module should
        # support multiple identities
        user.email = "chrismont@temple.edu";
      };
    in
    [
      {
        inherit contents;
        condition = "gitdir:~/Projects/work/";
        contentSuffix = "work-by-gitdir-gitconfig";
      }
      {
        inherit contents;
        condition = "hasconfig:remote.*.url:git@github.com:kleinweb/**";
        contentSuffix = "work-by-remote-url-gitconfig";
      }
    ];
}
