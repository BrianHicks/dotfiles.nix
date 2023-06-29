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

      -- which-key
      vim.o.timeout = true
      vim.o.timeoutlen = 300
      require("which-key").setup()

      -- git
      require('gitsigns').setup()

      vim.keymap.set('n', '<leader>gg', ':Git<CR>', { desc = '[G]it overview' })
      vim.keymap.set('n', '<leader>gb', require('gitsigns').toggle_current_line_blame, { desc = 'Toggle [G]it [B]lame' })
      vim.keymap.set('n', '<leader>gh', require('gitsigns').preview_hunk_inline, { desc = 'Preview [G]it [H]unk' })
      vim.keymap.set('n', '<leader>gs', require('gitsigns').stage_hunk, { desc = '[G]it [S]tage hunk' })
      vim.keymap.set('n', '<leader>gS', require('gitsigns').stage_buffer, { desc = '[G]it [S]tage buffer' })
      vim.keymap.set('n', '<leader>gc', ':Git commit<CR>', { desc = '[G]it [c]ommit' })

      vim.keymap.set('n', '{', require('gitsigns').prev_hunk)
      vim.keymap.set('n', '}', require('gitsigns').next_hunk)
    '';

    plugins = with pkgs.vimPlugins; [
      better-escape-nvim
      which-key-nvim

      # Git
      vim-fugitive
      vim-rhubarb
      gitsigns-nvim

      # Detect tabs and shiftwidth automatically
      vim-sleuth
    ];
  };
}
