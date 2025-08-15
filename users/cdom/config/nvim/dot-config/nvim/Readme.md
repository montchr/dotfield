# Neovim

## Plugins

### `impatient.nvim`

[lewis6991/impatient.nvim: Improve startup time for Neovim][impatient-repo]

- Chunk caching of Lua modules to bytecode,
  invalidated via hash check
  based on modtime+fsize
  :: `$XDG_CACHE_HOME/nvim/luacache_chunks`
- Module resolution cache
  mapping name to path
  :: `$XDG_CACHE_HOME/nvim/luacache_modpaths`

[impatient-repo]: https://github.com/lewis6991/impatient.nvim/

#### Caveats

- May introduce non-deterministic load order in modpath cache

### `nvim-lua/plenary.nvim`

<https://github.com/nvim-lua/plenary.nvim>

> plenary:
>
>     full; complete; entire; absolute; unqualified.

- `plenary.async`
- `plenary.async_lib`
- `plenary.job`
- `plenary.path`
- `plenary.scandir`
- `plenary.context_manager`
- `plenary.test_harness`
- `plenary.filetype`
- `plenary.strings`

## Fennel

### Preferred: `Olical/aniseed`

[Olical/aniseed: Neovim configuration and plugins in Fennel (Lisp compiled to Lua)][aniseed]

- [Olical/magic-kit: A starter kit for Conjure, Aniseed and Neovim][magic-kit]
- [dotfiles/stowed/.config/nvim at main · Olical/dotfiles · GitHub][olical-dotfiles]

[aniseed]: https://github.com/Olical/aniseed
[magic-kit]: https://github.com/Olical/magic-kit/
[olical-dotfiles]: https://github.com/Olical/dotfiles/tree/main/stowed/.config/nvim

### Alternative: Hotpot

<https://github.com/rktjmp/hotpot.nvim>

See [Reddit conversation][hotpot-reddit-thread]
between the author of nyoom.vim and the author of Hotpot.

[hotpot-reddit-thread]: https://www.reddit.com/r/neovim/comments/souj2j/comment/hwbhm6p/

#### Caveats

- May require numerous macro functions for a real benefit over Aniseed,
  which already provides its own set of macros.
  The Hotpot configurations I've come across all define their own set of similarly-named copypasta macros,
  seemingly copied over from the macros defined in [`datwaft/themis.nvim`][themis-repo].

[themis-repo]: https://github.com/datwaft/themis.nvim

## Inspiration

- [Mayooonaiselol/Daydream.nvim][daydream-repo]
- [shaunsingh/nyoom.nvim][nyoom-repo]

[daydream-repo]: https://github.com/Mayooonaiselol/Daydream.nvim
[nyoom-repo]: https://github.com/shaunsingh/nyoom.nvim
