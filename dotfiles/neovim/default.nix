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
        vim.cmd [[source ${./lsp.lua}]]
        vim.cmd [[source ${./lualine.lua}]]
        vim.cmd [[source ${./prosession.lua}]]
        vim.cmd [[source ${./startify.vim}]]
        vim.cmd [[source ${./telescope.lua}]]
        vim.cmd [[source ${./testing.lua}]]
        vim.cmd [[source ${./toggleterm.lua}]]
        vim.cmd [[source ${./trouble.lua}]]

        -- language servers
        local lspconfig = require('lspconfig')

        local capabilities = vim.lsp.protocol.make_client_capabilities()
        capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

        lspconfig.elmls.setup({
          cmd = { "${pkgs.elmPackages.elm-language-server}/bin/elm-language-server" },
          capabilities = capabilities,
        })
        lspconfig.rust_analyzer.setup({
          capabilities = capabilities,
          settings = {
            ["rust-analyzer"] = {
              files = {
                excludeDirs = {
                  -- rust-analyzer wants to look in .direnv by default. Problem
                  -- is, that's where all the Nix stuff lives and it's a huuuuuuge
                  -- directory to scan. Stop it, rust-analyzer!
                  ".direnv",
                },
              },
            },
          },
          on_attach = function(client, bufnr)
            vim.lsp.inlay_hint.enable(bufnr)
          end,
        })
        lspconfig.sorbet.setup({
          cmd = { "bundle", "exec", "srb", "typecheck", "--lsp", "--enable-all-beta-lsp-features" },
          capabilities = capabilities,
        })
        lspconfig.tsserver.setup({
          cmd = {
            "${pkgs.nodePackages.typescript-language-server}/bin/typescript-language-server",
            "--stdio",
          },
          capabilities = capabilities
        })
        lspconfig.nil_ls.setup({
          cmd = { "${pkgs.nil}/bin/nil" },
          capabilities = capabilities,
        })
        lspconfig.pyright.setup({ capabilities = capabilities })
        lspconfig.svelte.setup({ capabilities = capabilities })
      '';

    plugins = with pkgs.vimPlugins; [
      better-escape-nvim
      comment-nvim
      vim-sensible
      vim-tmux-navigator
      which-key-nvim

      # Text editing (wait isn't that just all of vim?)
      fidget-nvim
      formatter-nvim
      nvim-lspconfig
      nvim-surround
      nvim-treesitter.withAllGrammars
      trouble-nvim
      vim-sleuth
      vim-unimpaired

      # Completion and snippets
      cmp-buffer
      cmp-cmdline
      cmp-git
      cmp-nvim-lsp
      cmp-path
      cmp_luasnip
      friendly-snippets
      luasnip
      nvim-cmp

      # Navigation
      nvim-web-devicons
      other-nvim
      telescope-fzf-native-nvim
      telescope-nvim
      telescope-ui-select-nvim
      toggleterm-nvim
      vim-eunuch
      vim-obsession
      vim-prosession
      vim-startify
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
