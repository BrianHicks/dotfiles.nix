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
      ''
        vim.cmd [[source ${./init.lua}]]

        vim.cmd [[source ${./alternate.lua}]]
        vim.cmd [[source ${./autoformat.lua}]]
        vim.cmd [[source ${./git.lua}]]
        vim.cmd [[source ${./telescope.lua}]]
        vim.cmd [[source ${./testing.lua}]]
        vim.cmd [[source ${./trouble.lua}]]
        vim.cmd [[source ${./toggleterm.lua}]]

        -- language servers
        local lspconfig = require('lspconfig')

        local capabilities = vim.lsp.protocol.make_client_capabilities()
        capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

        lspconfig.elmls.setup({
          cmd = { "${pkgs.elmPackages.elm-language-server}/bin/elm-language-server" },
          capabilities = capabilities,
        })
        lspconfig.rust_analyzer.setup({ capabilities = capabilities })
        lspconfig.sorbet.setup({
          cmd = { "bundle", "exec", "srb", "typecheck", "--lsp", "--enable-all-beta-lsp-features" },
          capabilities = capabilities,
        })
        lspconfig.tsserver.setup({ capabilities = capabilities })
        lspconfig.nil_ls.setup({
          cmd = { "${pkgs.nil}/bin/nil" },
          capabilities = capabilities,
        })
        lspconfig.dafny.setup({
          capabilities = capabilities,
        })
      '';

    plugins = with pkgs.vimPlugins; [
      better-escape-nvim
      which-key-nvim
      comment-nvim
      vim-tmux-navigator

      # Text editing (wait isn't that just all of vim?)
      fidget-nvim
      nvim-lspconfig
      nvim-surround
      nvim-treesitter.withAllGrammars
      vim-sleuth
      trouble-nvim

      # Completion and snippets
      cmp-buffer
      cmp-cmdline
      cmp-nvim-lsp
      cmp-path
      cmp_luasnip
      friendly-snippets
      luasnip
      nvim-cmp

      # Navigation
      nvim-web-devicons
      other-nvim
      telescope-nvim
      telescope-ui-select-nvim
      toggleterm-nvim
      vim-eunuch
      vim-vinegar

      # Git
      vim-fugitive
      vim-gitgutter
      vim-rhubarb

      # Testing and Running
      vim-dispatch
      vim-test

      # themes and visual niceties
      nightfox-nvim
      lualine-nvim
    ];
  };
}
