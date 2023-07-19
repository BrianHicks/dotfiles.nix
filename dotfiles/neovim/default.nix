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

      vim.cmd [[source ${./neo-tree.lua}]]
      vim.cmd [[source ${./telescope.lua}]]
      vim.cmd [[source ${./trouble.lua}]]
      vim.cmd [[source ${./autoformat.lua}]]

      -- language servers
      local lspconfig = require('lspconfig')

      local capabilities = vim.lsp.protocol.make_client_capabilities()
      capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

      lspconfig.elmls.setup({
        cmd = { "${pkgs.elmPackages.elm-language-server}/bin/elm-language-server" },
        capabilities = capabilities,
      })
      lspconfig.rust_analyzer.setup({ capabilities = capabilities })
      print(lspconfig.sorbet.setup({
        cmd = { "bundle", "exec", "srb", "typecheck", "--lsp", "--enable-all-beta-lsp-features" },
        capabilities = capabilities,
      }))
      lspconfig.tsserver.setup({ capabilities = capabilities })
      lspconfig.nil_ls.setup({
        cmd = { "${pkgs.nil}/bin/nil" },
        capabilities = capabilities,
      })
    '';

    plugins = with pkgs.vimPlugins; [
      better-escape-nvim
      which-key-nvim
      comment-nvim

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
      telescope-nvim
      neo-tree-nvim
      nvim-web-devicons

      # Git
      gitsigns-nvim
      vim-fugitive
      vim-rhubarb

      # themes and visual niceties
      nightfox-nvim
      lualine-nvim
    ];
  };
}
