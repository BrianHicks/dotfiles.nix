{ config, ... }:
{
  programs.jujutsu = {
    enable = true;

    settings = {
      user.name = config.programs.git.settings.user.name;
      user.email = config.programs.git.settings.user.email;

      aliases = {
        # Aliases from Artem that haven't entered my muscle memory yet
        log-recent = [
          "log"
          "-r"
          "default() & recent()"
        ];
        tug = [
          "bookmark"
          "move"
          "--from"
          "closest_bookmark(@-)"
          "--to"
          "@-"
        ];
        c = [ "commit" ];
        ci = [
          "commit"
          "--interactive"
        ];
        e = [ "edit" ];
        i = [
          "git"
          "init"
          "--colocate"
        ];
        nb = [
          "bookmark"
          "create"
          "-r @-"
        ];
        pull = [
          "git"
          "fetch"
        ];
        push = [
          "git"
          "push"
        ];
        r = [ "rebase" ];
        s = [ "squash" ];
        si = [
          "squash"
          "--interactive"
        ];

        # from converations, but still not in muscle memory
        unify = [
          "new"
          "-r"
          "tracked_remote_bookmarks() & mine()"
        ];
        rebase-all = [
          "rebase"
          "-s"
          "roots(trunk()..@)"
          "-d"
          "trunk()"
        ];
      };
    };
  };

  programs.jjui = {
    enable = true;
  };
}
