-- LSP
-- NOTE: language servers are set up in default.nix!

vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = 'Go to previous diagnostic message' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = 'Go to next diagnostic message' })
vim.keymap.set('n', '<leader>d', vim.diagnostic.open_float, { desc = 'Open floating diagnostic message' })

vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    local nmap = function(keys, func, desc)
      if desc then
        desc = 'LSP: ' .. desc
      end

      vim.keymap.set('n', keys, func, { buffer = ev.buf, desc = desc })
    end

    -- Buffer-local mappings
    nmap('<leader>r', vim.lsp.buf.rename, '[R]ename')
    nmap('<leader>a', vim.lsp.buf.code_action, 'Code [a]ction')

    nmap('gd', vim.lsp.buf.definition, '[G]oto [D]efinition')
    nmap('gr', '<cmd>Telescope lsp_references<CR>', '[G]oto [R]eferences')
    nmap('gI', vim.lsp.buf.implementation, '[G]oto [I]mplementation')
    nmap('gD', vim.lsp.buf.type_definition, '[G]oto type [D]efinition')
    nmap('<leader>s', '<cmd>Telescope lsp_document_symbols<CR>', 'Document [S]ymbols')
    nmap('<leader>S', '<cmd>Telescope lsp_dynamic_workspace_symbols<CR>', 'Workspace [S]ymbols')
    nmap('K', vim.lsp.buf.hover, 'Hover documentation')

    vim.api.nvim_buf_create_user_command(ev.buf, 'Format', function(_)
      vim.lsp.buf.format()
    end, { desc = 'Format current buffer with LSP' })

   require("fidget").setup({})
 end,
})
