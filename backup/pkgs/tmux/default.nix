{ sources ? import ../../nix/sources.nix { }, pkgs ? import sources.nixpkgs { }
}:

# macOS does some weird stuff with locales and character widths. Practicaly, that
# means that without extra support tmux will behave weirdly around multi-byte
# characters like emoji. Enabling utf8proc support should backfill the right
# tables so that tmux can get the correct character widths.
pkgs.tmux.overrideAttrs (attrs:
  attrs // {
    src = sources.tmux;
    buildInputs = attrs.buildInputs ++ [ pkgs.utf8proc ];
    configureFlags = attrs.configureFlags ++ [ "--enable-utf8proc" ];
  })
