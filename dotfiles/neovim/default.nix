{ pkgs, ... }:
{
  # resources:
  #
  # - https://github.com/nunocf/home-manager-nix/blob/master/nvim/nvim.nix
  # - https://github.com/nvim-lua/kickstart.nvim/blob/master/init.lua#L197-L198
  #
  programs.neovim = {
    enable = true;
    vimAlias = true;

    extraLuaConfig =
      # lua
      ''
      -- Leaders need to be mapped before requiring anything so they will be
      -- correctly bound everywhere.
      vim.g.mapleader = ' '
      vim.g.maplocalleader = ' '

      -- misc options
      vim.wo.number = true
      vim.o.mouse = 'a'
      vim.o.breakindent = true
      vim.o.ignorecase = true
      vim.o.smartcase = true
      vim.wo.signcolumn = 'yes'
      vim.o.termguicolors = true

      -- swap ; and : to save a keystroke and reduce wear on the pinky finger
      vim.keymap.set('n', ';', ':')
      vim.keymap.set('n', ':', ';')

      require("better_escape").setup {
        mapping = {"fd"},
	-- TODO: clear_empty_lines?
      }

      -- make an easier way to exit insert and write
      vim.keymap.set('i', '<C-e>', '<esc>:w<cr>') -- TODO: do :wq in a fugitive commit buffer

      -- which-key
      vim.o.timeout = true
      vim.o.timeoutlen = 300
      require("which-key").setup()

      -- comments
      require('Comment').setup()

      -- theming
      vim.cmd('colorscheme nightfox')

      -- git
      require('gitsigns').setup()

      vim.keymap.set('n', '<leader>gs', ':Git<CR>', { desc = '[G]it overview' })
      vim.keymap.set('n', '<leader>gb', require('gitsigns').toggle_current_line_blame, { desc = 'Toggle [G]it [B]lame' })
      vim.keymap.set('n', '<leader>gh', require('gitsigns').preview_hunk_inline, { desc = 'Preview [G]it [H]unk' })
      vim.keymap.set('n', '<leader>gs', require('gitsigns').stage_hunk, { desc = '[G]it [S]tage hunk' })
      vim.keymap.set('n', '<leader>gS', require('gitsigns').stage_buffer, { desc = '[G]it [S]tage buffer' })
      vim.keymap.set('n', '<leader>gc', ':Git commit -v<CR>', { desc = '[G]it [c]ommit' })
      vim.keymap.set('n', '<leader>gC', ':Git commit -m ""<Left>', { desc = '[G]it [C]ommit inline' })

      vim.keymap.set('n', '{', require('gitsigns').prev_hunk)
      vim.keymap.set('n', '}', require('gitsigns').next_hunk)

      -- surround
      require('nvim-surround').setup()
    '';

    plugins = with pkgs.vimPlugins; [
      better-escape-nvim
      which-key-nvim
      comment-nvim

      # Text editing (wait isn't that just all of vim?)
      nvim-surround

      # Git
      vim-fugitive
      vim-rhubarb
      gitsigns-nvim

      # Detect tabs and shiftwidth automatically
      vim-sleuth

      # themes and visual niceties
      nightfox-nvim
    ];
  };
}
