require("other-nvim").setup({
  mappings = {
    -- builtins
    "rails",

    -- custom
  },
})

vim.api.nvim_set_keymap("n", "ga", "<cmd>:Other<CR>", { noremap = true, silent = true })
