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

      -- git
      require('gitsigns').setup()

      vim.keymap.set('n', '<leader>gs', ':Git<CR>', { desc = '[G]it [S]tatus' })
      vim.keymap.set('n', '<leader>gb', ':Gitsigns toggle_current_line_blame<CR>', { desc = 'Toggle [G]it [B]lame' })
      vim.keymap.set('n', '<leader>gh', ':Gitsigns preview_hunk_inline<CR>', { desc = 'Preview [G]it [H]unk' })
      vim.keymap.set('n', '<leader>gs', ':Gitsigns stage_hunk<CR>', { desc = '[G]it [S]tage hunk' })
      vim.keymap.set('n', '<leader>gS', ':Gitsigns stage_buffer<CR>', { desc = '[G]it [S]tage buffer' })

      vim.keymap.set('n', '{', ':Gitsigns prev_hunk<CR>')
      vim.keymap.set('n', '}', ':Gitsigns next_hunk<CR>')
    '';

    plugins = with pkgs.vimPlugins; [
      better-escape-nvim

      # Git
      vim-fugitive
      vim-rhubarb
      gitsigns-nvim

      # Detect tabs and shiftwidth automatically
      vim-sleuth
    ];
  };
}
