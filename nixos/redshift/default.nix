{ ... }: {
  # TODO: it'd be a little nicer to use geoclue to do this but the permissions
  # model is a little odd without a desktop environment like Gnome. Surely
  # there's a way to get it to work, though!
  location.latitude = 38.788105;
  location.longitude = -90.497437;

  services.redshift.enable = true;
}
