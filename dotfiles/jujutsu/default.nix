{ ... }: {
  programs.jujutsu = {
    enable = true;

    settings = {
      user.name = "Brian Hicks";
      user.email = "brian@brianthicks.com";
    };
  };
}
