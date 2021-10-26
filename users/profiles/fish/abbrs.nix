{ config, ... }:
let inherit (config) my; in
{
  t = "tail -f";

  # Make it easy to copy/paste script commands verbatim
  # "$" = "";

  q = "exit";
  sudo = "sudo ";
  wget = "wget -c";

  rcpd = "rcp --delete --delete-after";
  rcpu = "rcp --chmod=go=";
  rcpdu = "rcpd --chmod=go=";

  g = "git";

  # add / branch / checkout / reset
  ga = "git add";
  gap = "git add -p";
  gb = "git branch";
  grs = "git reset";
  gco = "git checkout";
  gcob = "git checkout -b";

  # commit
  gc = "git commit -v";
  gca = "git commit -v --amend";
  # amend in place with no edits to message
  gcam = "git commit -v --amend -C HEAD";

  # pull / push
  gpl = "git pull";
  gplo = "git pull origin";
  gpls = "git pull && gsumo";
  gps = "git push";
  gpsu = "git push -u";
  gpso = "git push origin";
  gpss = "git push && gsumo";

  # status / diff / difftool
  gs = "git status";
  gd = "git diff";
  gdt = "git difftool";
  # Show list of files changed in a specified commit or other ref
  gdl = "git diff-tree --no-commit-id --name-only -r";

  # stash
  gsh = "git stash";
  gsha = "git stash apply";
  gshp = "git stash pop";
  gshl = "git stash list";

  # log
  gl = "git log --oneline --decorate -20";
  # List all the commits on the current branch ahead of master.
  # glb="git log --oneline --decorate ${GIT_PRIMARY_BRANCH:-main}..${GIT_BRANCH_NAME:-HEAD}"

  # submodule
  # Magical fix for all submodule issues.
  gsumo = "git submodule update - -init - -recursive";


  # -------------------------------------
  #  NPM
  # -------------------------------------

  nrb = "npm run build";
  nrd = "npm run dev";


  # -------------------------------------
  #  VAGRANT
  # -------------------------------------

  vup = "vagrant up";
  vre = "vagrant reload";
}
