require("other-nvim").setup({
  mappings = {
    -- builtins
    "rails",

    -- custom
  },
})

vim.api.nvim_set_keymap("n", "ga", "<cmd>:Other<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "gA", "<cmd>:OtherVSplit<CR>", { noremap = true, silent = true })
