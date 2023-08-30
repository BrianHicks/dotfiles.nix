require("other-nvim").setup({
  mappings = {
    -- builtins
    "rails",

    -- the "going back to source from tests" from the Rails builtin but with a _spec suffix
    {
      pattern = "(.+)/spec/(.*)/(.*)/(.*)_spec.rb",
      target = {
        { target = "%1/db/%3/%4.rb" },
        { target = "%1/app/%3/%4.rb" },
        { target = "%1/%3/%4.rb" },
      },
    },
    {
      pattern = "(.+)/spec/(.*)/(.*)_spec.rb",
      target = {
        { target = "%1/db/%2/%3.rb" },
        { target = "%1/app/%2/%3.rb" },
        { target = "%1/lib/%2/%3.rb" },
      },
    },
    {
      pattern = "(.+)/spec/(.*)/(.*)_(.*)_spec.rb",
      target = {
        { target = "%1/app/%4s/%3_%4.rb" },
      },
    }, 
    {
      pattern = "/lib/(.*)/(.*).rb",
      target = "/spec/lib/%1/%2_spec.rb",
      context = "test",
    },
  },
})

vim.api.nvim_set_keymap("n", "ga", "<cmd>:Other<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "gA", "<cmd>:OtherVSplit<CR>", { noremap = true, silent = true })
