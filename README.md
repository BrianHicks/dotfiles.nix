# Dotfiles

My dotfiles.

1. check this out to `~/code/brian/dotfiles.nix`
2. install nix
3. install nix-darwin
4. run `./darwin.sh switch`

## Fixes

### Root Channels

If Nix says:

```
warning: Nix search path entry '/nix/var/nix/profiles/per-user/root/channels' does not exist, ignoring
```

```sh
mkdir -p /nix/var/nix/profiles/per-user/root
touch /nix/var/nix/profiles/per-user/root/channels
```

### Terminfo

If `lazygit` doesn't load correctly, or Kakoune colors look weird, load the terminfo:

```sh
tic manual-fixes/tmux-256color.terminfo
```

(It's vendored from the Kakoune contrib source tree.)
