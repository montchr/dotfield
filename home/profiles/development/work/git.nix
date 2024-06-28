{
  # TIP: verify with e.g.:
  # $ fd -t d '\.git$' -H -x bash -c 'cd {//} && git config --get user.email'
  programs.git.includes =
    let
      contents = {
        # FIXME: add this to an identity config -- dotfield's whoami module
        # should support multiple identities
        user.email = "chrismont@temple.edu";
      };
    in
    [
      {
        inherit contents;
        condition = "gitdir:~/Projects/work/";
        contentSuffix = "work-by-gitdir-gitconfig";
      }
    ];
}
