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
      local gitsigns = require('gitsigns')
      gitsigns.setup()
      gitsigns.toggle_numhl()

      vim.keymap.set('n', '<leader>gs', ':Git<CR>', { desc = '[G]it overview' })
      vim.keymap.set('n', '<leader>gb', ':Telescope git_branches<CR>', { desc = 'View [G]it [B]ranches' })
      vim.keymap.set('n', '<leader>gh', gitsigns.preview_hunk_inline, { desc = 'Preview [G]it [H]unk' })
      vim.keymap.set('n', '<leader>gw', gitsigns.stage_hunk, { desc = '[G]it [S]tage hunk' })
      vim.keymap.set('n', '<leader>gW', gitsigns.stage_buffer, { desc = '[G]it [S]tage buffer' })
      vim.keymap.set('n', '<leader>gu', gitsigns.undo_stage_hunk, { desc = '[G]it [U]ndo stage hunk' })
      vim.keymap.set('n', '<leader>gC', ':Git commit -v<CR>', { desc = '[G]it [c]ommit' })
      vim.keymap.set('n', '<leader>gc', ':Git commit -m ""<Left>', { desc = '[G]it [C]ommit inline' })
      vim.keymap.set('n', '<leader>gr', ':Telescope git_bcommits<CR>', { desc = '[G]it [R]evert to commit' })

      vim.keymap.set('n', '{', require('gitsigns').prev_hunk)
      vim.keymap.set('n', '}', require('gitsigns').next_hunk)

      -- surround
      require('nvim-surround').setup()

      -- tree-sitter
      require('nvim-treesitter.configs').setup {
        -- grammars are taken care of by Nix
        highlight = { enable = true },
        incremental_selection = { enable = true },
        textobjects = { enable = true },
      }

      -- telescope
      vim.keymap.set('n', '-', ':Telescope find_files<CR>', { desc = 'find files' })
      vim.keymap.set('n', '_', ':Telescope buffers<CR>', { desc = '[F]ind [B]uffers' })
      vim.keymap.set('n', '<leader>ff', ':Telescope resume<CR>', { desc = 'Resume last Telescope operation' })
      vim.keymap.set('n', '<leader>f/', ':Telescope current_buffer_fuzzy_find<CR>', { desc = '[F]ind by searching' })
      vim.keymap.set('n', '<leader>fp', ':Telescope live_grep<CR>', { desc = '[F]ind in [P]roject' })
      vim.keymap.set('n', '<leader>fa', ':Telescope builtin<CR>', { desc = '[F]ind [a]nything telescope can' })
      vim.keymap.set('n', '<leader>fh', ':Telescope help_tags<CR>', { desc = '[F]ind [H]elp tags' })
    '';

    plugins = with pkgs.vimPlugins; [
      better-escape-nvim
      which-key-nvim
      comment-nvim

      # Text editing (wait isn't that just all of vim?)
      nvim-surround
      nvim-treesitter.withAllGrammars
      vim-sleuth

      # Navigation
      telescope-nvim

      # Git
      vim-fugitive
      vim-rhubarb
      gitsigns-nvim

      # themes and visual niceties
      nightfox-nvim
    ];
  };
}
