local troubleBinding = function(binding, command)
  vim.keymap.set("n", binding, command, {silent = true, noremap = true})
end

troubleBinding("<leader>D", "<cmd>TroubleToggle<CR>")
troubleBinding("<leader>xw", "<cmd>TroubleToggle workspace_diagnostics<cr>")
troubleBinding("<leader>xd", "<cmd>TroubleToggle document_diagnostics<cr>")
troubleBinding("<leader>xl", "<cmd>TroubleToggle loclist<cr>")
troubleBinding("<leader>xq", "<cmd>TroubleToggle quickfix<cr>")
troubleBinding("gR", "<cmd>TroubleToggle lsp_references<cr>")
