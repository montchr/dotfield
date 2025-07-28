{ moduleWithSystem, ... }:
{
  dotfield.modules.defaults.home.programs.git = {
    userEmail = whoami.email;
    userName = whoami.name;
  };

  dotfield.modules.development.home = moduleWithSystem (
    perSystem@{ packages }:
    home@{ pkgs, ... }:
    {

      programs.git.extraConfig.github.user = "montchr";
      programs.gh.settings.git_protocol = "ssh";
      programs.gh.extensions = [
        packages.gh-repo-explore
        packages.gh-s

        pkgs.gh-dash
        pkgs.gh-eco
      ];

      extraConfig = {
        # Result: <short-sha> <commit-message> (<pointer-names>) -- <commit-author-name>; <relative-time>
        pretty.nice = "%C(yellow)%h%C(reset) %C(white)%s%C(cyan)%d%C(reset) -- %an; %ar";

        ## remotes
        fetch.recurseSubmodules = true;
        push.default = "current";
        apply.whitespace = "nowarn";
        pull.rebase = true;
      };
    }
  );
}
