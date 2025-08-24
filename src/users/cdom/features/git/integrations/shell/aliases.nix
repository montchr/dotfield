{ lib', ... }:
let
  abbrs = {
    g = "git";

    #### Working Copy:

    ga = "git add";
    gap = "git add -p";
    gb = "git branch";
    grs = "git reset";
    gco = "git checkout";
    gcob = "git checkout -b";

    #### Commits:

    gc = "git commit -v";
    gcm = "git commit -v -m";
    gca = "git commit -v --amend";
    # extend previous commit
    gce = "git commit -v --amend -C HEAD";

    #### Pull/Push:

    gpl = "git pull";
    gplo = "git pull origin";
    gps = "git push";
    gpsu = "git push -u";
    gpso = "git push origin";

    #### Status:

    gs = "git status";
    gd = "git diff";
    gdt = "git difftool";
    # Show list of files changed in a specified commit or other ref
    gdl = "git diff-tree --no-commit-id --name-only -r";

    #### Stash:

    gsh = "git stash";
    gsha = "git stash apply";
    gshp = "git stash pop";
    gshl = "git stash list";

    #### Log:

    gl = "git log --oneline --decorate -5";
    gll = "git log --oneline --decorate -10";
    glll = "git log --oneline --decorate -20";

    # List all the commits on the current branch ahead of master.
    # glb="git log --oneline --decorate ${GIT_PRIMARY_BRANCH:-main}..${GIT_BRANCH_NAME:-HEAD}"

    #### Submodule:

    # Magical fix for all submodule issues.
    gsumo = "git submodule update --init --recursive";
  };
in
{
  dotfield.users.cdom.aspects.development.home = (
    lib'.shell.makeShellAliasesModule {
      inherit abbrs;
    }
  );
}
