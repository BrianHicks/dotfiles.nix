{ pkgs, ... }: {
  # The idea here is to set a different k9s skin for each cluster I work in. I use
  # `builtins.readFile` here instead of setting `home.file.*.source` so it doesn't
  # fail silently if I make a typo.
  #
  # TODO: make this work for Linux too, if it's annoying that it doesn't. Just
  # need to use ~/.config instead?
  home.file."Library/Preferences/k9s/backyard_skin.yml".text =
    builtins.readFile "${pkgs.k9s-skins}/snazzy.yml";

  home.file."Library/Preferences/k9s/staging_skin.yml".text =
    builtins.readFile "${pkgs.k9s-skins}/monokai.yml";

  home.file."Library/Preferences/k9s/production_skin.yml".text =
    builtins.readFile "${pkgs.k9s-skins}/red.yml";
}
