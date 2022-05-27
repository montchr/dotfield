# montchr/dotfield

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

This is Dotfield, my worlds and systems, a nutrient-rich typo terraforming
the hungry heads of a talking planet.

Primarily for macOS right now, but gradually introducing NixOS
configurations as well.

## Identities

### SSH @ <https://meta.sr.ht/~montchr.keys>

Copy entries as needed from [`secrets/authorized-keys.nix`](./secrets/secrets.nix)

### GPG/PGP @ <https://meta.sr.ht/~montchr.pgp>

From https://github.com/drduh/YubiKey-Guide#using-keys:

```sh
export KEYID="0x135EEDD0F71934F3"
gpg --recv $KEYID
```

## Hydration

### Hetzner Cloud

```sh
curl -L https://raw.githubusercontent.com/montchr/dotfield/main/provision/bin/nixos-install-hetzner-cloud.sh | sudo bash
```

Adapted from the script provided by https://github.com/nix-community/nixos-install-scripts.

## Vertebrae

- https://github.com/divnix/digga :: a good friend

## Grafts

Generally in order of recency.

- https://github.com/Xe/nixos-configs
- https://github.com/sei40kr/dotfiles
- https://github.com/malob/nixpkgs
- https://github.com/kclejeune/system
- https://github.com/ahmedelgabri/dotfiles
- https://github.com/cmacrae/config
- https://github.com/hlissner/dotfiles
- https://github.com/d12frosted/environment
- https://github.com/hardselius/dotfiles
- https://github.com/alrra/dotfiles
- https://github.com/jasonheecs/ubuntu-server-setup

## Errata

### kitty terminal custom icons

kitty's FAQ page shows a small collection of high-quality alternative icons designed by some kitty fans.

<https://sw.kovidgoyal.net/kitty/faq/#i-do-not-like-the-kitty-icon>

While I personally don't _dislike_ the kitty icon, these alternatives are great.

- **Currently: <https://github.com/DinkDonk/kitty-icon>**
- <https://github.com/k0nserv/kitty-icon>
- <https://github.com/hristost/kitty-alternative-icon>

#### NixOS

TKTKTK

#### macOS

There's a couple hoops to jump through in order to specify a custom application icon.

And, unfortunately, because I currently install `kitty.app` with Homebrew due to
frequent build failures with `nixpkgs#kitty`, any customisations will be
reverted whenever the app updates.

I've added a script called `kitty-set-app-icon` to re-copy the desired icon back
to `kitty.app` post-update. This script is available via the
`kitty-helpers.setAppIcon` package.

Credit goes to [this blog post](https://www.sethvargo.com/replace-icons-osx/) for outlining a simple alternative to the usual drag-and-drop approach.
