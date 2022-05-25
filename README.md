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

#### macOS

Of course, there's a couple hoops to jump through in order to specify a custom application icon. Most tutorials out there repeat the same general instructions. Quoting from [one of the icon repos](https://github.com/DinkDonk/kitty-icon):

> 1. Find `kitty.app` in the `Applications` folder, select it and press `⌘ + i`.
> 2. Drag `kitty-dark.icns` or `kitty-light.icns` onto the application icon in the kitty info pane.
> 3. Delete the icon cache and restart `Dock`:
>
> ```sh
> $ rm /var/folders/*/*/*/com.apple.dock.iconcache; killall Dock
> ```

Unfortunately, any customisations will be reverted whenever the app updates.

For that reason, I'm taking the manual route for now. But in the future, I'd consider creating a =nix-darwin= module for customizing icons declaratively. But that ranks pretty low on my "nice to have" list.

[This blog post](https://www.sethvargo.com/replace-icons-osx/) outlines a simple alternative. In summary (though the path might not actually be correct):

```sh
cp ~/path/to/kitty.icns /Applications/kitty.app/Contents/Resources/kitty.icns
```
