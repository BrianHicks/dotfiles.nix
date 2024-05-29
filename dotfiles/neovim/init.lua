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
vim.api.nvim_set_option('updatetime', 750)
vim.o.undofile = true
vim.o.completeopt = 'menuone,noselect'

-- swap ; and : to save a keystroke and reduce wear on the pinky finger
vim.keymap.set('n', ';', ':')
vim.keymap.set('n', ':', ';')

-- swap ^ and 0 because I always want ^ but 0 is easier to press
vim.keymap.set('n', '^', '0')
vim.keymap.set('n', '0', '^')

-- bounce on leader to go back to alternate file
vim.keymap.set('n', '<leader><leader>', '<C-6>')

-- deal with word wrap nicely
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- stay in visual mode while indenting
vim.keymap.set('v', '<', '<gv')
vim.keymap.set('v', '>', '>gv')

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

-- surround
require('nvim-surround').setup()

-- tree-sitter
require('nvim-treesitter.configs').setup {
  -- grammars are taken care of by Nix
  highlight = { enable = true },
  incremental_selection = {
    enable = true,

    keymaps = {
      init_selection = "<M-o>",
      node_incremental = "<M-o>",
      scope_incremental = "<M-S-o>",
      node_decremental = "<M-i>",
    },
  },
  textobjects = { enable = true },

  -- the Markdown parser currently has some issue in my main work repo that
  -- makes neovim quit (segfault?) when it tries to load the README.
  disable = { "markdown" },
}

-- completion
local cmp = require('cmp')
local luasnip = require('luasnip')
require('luasnip.loaders.from_vscode').lazy_load()
luasnip.config.setup({})

cmp.setup({
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete {},
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_locally_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.locally_jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),
  }),
  sources = cmp.config.sources(
    {
      { name = 'nvim_lsp' },
      { name = 'luasnip' },
    },
    {
      { name = 'buffer' },
    }
  ),
})

-- get better completions in git commits
cmp.setup.filetype('gitcommit', {
  sources = cmp.config.sources(
    { { name = 'git' } },
    { { name = 'buffer' } }
  )
})
require("cmp_git").setup()

-- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline({ '/', '?' }, {
  mapping = cmp.mapping.preset.cmdline(),
  sources = { { name = 'buffer' } },
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources(
    { { name = 'path' } },
    { { name = 'cmdline' } }
  )
})

-- autopairs
require("nvim-autopairs").setup({})
local cmp_autopairs = require('nvim-autopairs.completion.cmp')
cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done())

-- highlight on yank
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})

-- quickfix
vim.keymap.set('n', '<leader>wq', '<cmd>copen<CR>', { desc = 'Open quickfix' })
