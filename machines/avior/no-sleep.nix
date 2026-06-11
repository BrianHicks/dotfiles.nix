{ ... }:
{
  # Stop the computer from going to sleep while I'm trying to run things on it.
  systemd.sleep.settings.Sleep = {
    AllowHibernation = "no";
    AllowHybridSleep = "no";
    AllowSuspend = "no";
    AllowSuspendThenHibernate = "no";
  };
}
