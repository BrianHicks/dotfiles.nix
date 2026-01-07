{ config, ... }:
{
  programs.jujutsu = {
    enable = true;

    settings = {
      user.name = config.programs.git.settings.user.name;
      user.email = config.programs.git.settings.user.email;
    };
  };

  programs.jjui = {
    enable = true;
  };
}
