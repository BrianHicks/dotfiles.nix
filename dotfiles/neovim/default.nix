{ pkgs, ... }:
{
  programs.neovim = {
    enable = true;
    vimAlias = true;

    extraLuaConfig = ''
      -- Leaders need to be mapped before requiring anything so they will be
      -- correctly bound everywhere.
      vim.g.mapleader = ' '
      vim.g.maplocalleader = ' '

      -- swap ; and : to save a keystroke and reduce wear on the pinky finger
      vim.keymap.set('n', ';', ':')
      vim.keymap.set('n', ':', ';')

      require("better_escape").setup {
        mapping = {"fd"},
	-- TODO: clear_empty_lines?
      }
    '';

    plugins = with pkgs.vimPlugins; [
      better-escape-nvim
    ];
  };
}
