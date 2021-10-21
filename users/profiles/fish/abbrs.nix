{
  # l = "exa --color=auto -G";
  # ll = "exa --color=auto -Gla";
  # pbcopy = "xclip -sel clipboard";
  # pbpaste = "xclip -sel clipboard -o";

  t = "tail -f";

  # Make it easy to copy/paste script commands verbatim
  # "$" = "";

  q = "exit";
  clr = "clear";
  sudo = "sudo ";
  mkdir = "mkdir -pv";
  wget = "wget -c";

  rcpd = "rcp --delete --delete-after";
  rcpu = "rcp --chmod=go=";
  rcpdu = "rcpd --chmod=go=";

  # Use Kitty terminal"s ssh helper kitten
  sshk = "kitty +kitten ssh -o SendEnv=CDOM_OS_APPEARANCE -A";
  # Display an image in kitty
  icat = "kitty +kitten icat";

  # Always enable colored `grep` output
  # Note: `GREP_OPTIONS="--color=auto"` is deprecated, hence the alias usage.
  grep = "grep --color=auto";
  fgrep = "fgrep --color=auto";
  egrep = "egrep --color=auto";

  ".." = "cd ..";
  "..." = "cd ../..";
  "...." = "cd ../../..";
  "....." = "cd ../../../..";
  "......" = "cd ../../../../..";


  exa = "exa --color always --group-directories-first --git";
  l = "exa";
  ll = "exa --classify --group --icons --oneline";
  # `--all --all` is intentional -- it includes the `.` and `..` directories.
  la = "exa --all --all --classify --extended --header --long";
  ld = "exa --oneline --only-dirs";
  lld = "exa --all --classify --git --group --group-directories-first --header --long";
  tree = "exa --tree";


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


  # IP addresses
  ip = "dig +short myip.opendns.com @resolver1.opendns.com";
  localip = "ipconfig getifaddr en1";

  # Flush DNS cache
  flushdns = "dscacheutil -flushcache";

  # Empty the Trash on all mounted volumes and the main HDD
  # Also, clear Apple’s System Logs to improve shell startup speed
  emptytrash = "sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl";


  # -------------------------------------
  #  NPM
  # -------------------------------------

  nrb = "npm run build";
  nrd = "npm run dev";


  # -------------------------------------
  #  VAGRANT
  # -------------------------------------

  vup = "vagrant up";
}
