{ config, lib, pkgs, ... }:
let inherit (config) my; in
lib.mkMerge [
  {
    mkdir = "mkdir -pv";

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

    l = "exa";
    ll = "exa --classify --group --icons --oneline";
    # `--all --all` is intentional -- it includes the `.` and `..` directories.
    la = "exa --all --all --classify --extended --header --long";
    ld = "exa --oneline --only-dirs";
    lld = "exa --all --classify --git --group --group-directories-first --header --long";
    exa = "exa --color always --group-directories-first --git";
    tree = "exa --tree";

    # IP addresses
    ip = "dig +short myip.opendns.com @resolver1.opendns.com";
    localip = "ipconfig getifaddr en1";

    # Flush DNS cache
    flushdns = "dscacheutil -flushcache";

    # Empty the Trash on all mounted volumes and the main HDD
    # Also, clear Apple’s System Logs to improve shell startup speed
    emptytrash = "sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl";
  }
  (if pkgs.stdenv.isDarwin then { } else {
    pbcopy = "xclip -sel clipboard";
    pbpaste = "xclip -sel clipboard -o";
  })
]
