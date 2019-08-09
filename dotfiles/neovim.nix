{ pkgs, ... }:

let
  plugins = pkgs.callPackage ./neovim/plugins.nix { };

  brian-nur = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "BrianHicks";
    repo = "nur-packages";
    rev = "671f226ad54421832183eaa10c0b71d03dc4d7a3";
    sha256 = "1b2hfrnvsix0j0s3jlhhgqj6pgfgl0bx5j6gy9cr3kgv46ffgggi";
  }) { };

  similar-sort = pkgs.callPackage ../pkgs/similar-sort { };
in {
  programs.neovim = {
    enable = true;

    # aliases
    viAlias = true;
    vimAlias = true;

    # runtimes
    withNodeJs = true;

    configure = {
      customRC = ''
        inoremap fd <ESC>
        nnoremap : ;
        nnoremap ; :

        let mapleader = " "
        let localleader = ","

        " mouse support
        set mouse=a

        " splits
        nnoremap <c-j> <c-w><c-j>
        nnoremap <c-k> <c-w><c-k>
        nnoremap <c-l> <c-w><c-l>
        nnoremap <c-h> <c-w><c-h>

        set splitbelow
        set splitright

        " show stuff in the gutter
        set signcolumn=yes
        set number

        " colors
        " note: possibly need instructions at https://github.com/rakr/vim-one if I ever add tmux stuff
        set termguicolors
        set background=dark
        let g:one_allow_italics = 1
        colorscheme one

        " delimiters
        let delimitMate_expand_space=1
        let delimitMate_expand_cr=1
        let delimitMate_nesting_quotes = ['"', '`']
        let backspace=2

        " automatically change working directory when editing a file, so :edit
        " and friends work relatively. Causes problems with :make, which I will
        " fix later.
        " TODO: fix this to always work
        nnoremap <leader>cd :lcd %:p:h<CR>

        " automatically make missing directories on write
        augroup Mkdir
          autocmd!
          autocmd BufWritePre *
            \ if !isdirectory(expand("<afile>:p:h")) |
                \ call mkdir(expand("<afile>:p:h"), "p") |
            \ endif
        augroup END

        " fzf

        " don't show fzf statusline
        autocmd! FileType fzf
        autocmd  FileType fzf set laststatus=0 noshowmode noruler
          \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

        " bindings for fuzzy-finding
        nnoremap <silent> <leader>ff :call fzf#run(fzf#wrap({"source": "git ls-files \| ${similar-sort}/bin/similar-sort " . @%,
                                                           \ "sink": "edit",
                                                           \ "options": "--tiebreak index"
                                                           \ }))<CR>

        nnoremap <leader>fF :Files<CR>
        nnoremap <leader>bb :Buffers<CR>
        nnoremap <leader>fs :Ag<CR>
        nnoremap <leader>fl :Lines<CR>
        nnoremap <leader>fh :History<CR>
        nnoremap <leader>fc :Commits<CR>
        nnoremap <leader>fC :BCommmits<CR>
        nnoremap <leader>fH :Helptags<CR>

        " statusline
        let g:lightline = {
            \ 'colorscheme': 'one'
            \ }
        set noshowmode

        " language server
        set cmdheight=2
        set updatetime=300
        set shortmess+=c

        " https://github.com/neoclide/coc.nvim#example-vim-configuration
        " Use tab for trigger completion with characters ahead and navigate.
        " Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
        inoremap <silent><expr> <TAB>
              \ pumvisible() ? "\<C-n>" :
              \ <SID>check_back_space() ? "\<TAB>" :
              \ coc#refresh()
        inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

        function! s:check_back_space() abort
          let col = col('.') - 1
          return !col || getline('.')[col - 1]  =~# '\s'
        endfunction

        " Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
        " Coc only does snippet and additional edit on confirm.
        " inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

        " Use `[c` and `]c` to navigate diagnostics
        nmap <silent> [c <Plug>(coc-diagnostic-prev)
        nmap <silent> ]c <Plug>(coc-diagnostic-next)

        " Remap keys for gotos
        " TODO: find a way to only call these if the language server is active
        nmap <silent> gd <Plug>(coc-definition)
        nmap <silent> gy <Plug>(coc-type-definition)
        nmap <silent> gi <Plug>(coc-implementation)
        nmap <silent> gr <Plug>(coc-references)

        " Use K to show documentation in preview window
        nnoremap <silent> K :call <SID>show_documentation()<CR>

        function! s:show_documentation()
          if (index(['vim','help'], &filetype) >= 0)
            execute 'h '.expand('<cword>')
          else
            call CocAction('doHover')
          endif
        endfunction

        let g:coc_global_extensions = [ 'coc-git', 'coc-json', 'coc-yaml', 'coc-html', 'coc-vimlsp' ]

        nnoremap <leader>lR :CocRestart<CR>
        nnoremap <leader>la :CocAction<CR>
        nnoremap <leader>lc :CocCommand<CR>
        nnoremap <leader>lf :CocFix<CR>
        nnoremap <leader>li :CocInfo<CR>
        nnoremap <leader>ll :CocList<CR>
        nnoremap <leader>lo :CocOpenLog<CR>
        nnoremap <leader>lr :CocListResume<CR>

        augroup mygroup
          autocmd!
          " Setup formatexpr specified filetype(s).
          autocmd FileType elm setl formatexpr=CocAction('formatSelected')
          " Update signature help on jump placeholder
          autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
        augroup end

        " git
        nmap [g <Plug>(coc-git-prevchunk)
        nmap ]g <Plug>(coc-git-nextchunk)
        nmap gs <Plug>(coc-git-chunkinfo)
        nmap gc :Gcommit<CR>

        nnoremap <leader>gb :Gblame<CR>
        nnoremap <leader>gC :CocCommand git.copyUrl<CR>
        nnoremap <leader>gd :CocCommand git.diffCached<CR>
        nnoremap <leader>gf :CocCommand git.foldUnchanged<CR>
        nnoremap <leader>go :CocCommand git.browserOpen<CR>
        nnoremap <leader>gs :Gstatus<CR>

        nnoremap <leader>gr :Gmove
        nnoremap <leader>gR :Gremove<CR>
        nnoremap <leader>gw :CocCommand git.chunkStage<CR>
        nnoremap <leader>gW :Gwrite<CR>
        nnoremap <leader>gX :CocCommand git.chunkUndo<CR>

        nnoremap <leader>gb :CocList branches<CR>
        nnoremap <leader>gl :Glog<CR>

        " editing
        nnoremap <leader>er <Plug>(coc-rename)
        xmap <leader>ef <Plug>(coc-format)
        nmap <leader>ef <Plug>(coc-format)

        nnoremap <leader>eD :Delete<CR>
        nnoremap <leader>er :Rename
        nnoremap <leader>ec :Chmod
        nnoremap <leader>em :Mkdir
      '';

      packages.myVimPackage.start = plugins.all ++ [ pkgs.fzf ];
    };
  };

  home.file.".config/nvim/coc-settings.json".text = ''
    {
      "languageserver": {
        "elmLS": {
          "command": "${brian-nur.elm-language-server}/bin/elm-language-server",
          "args": ["--stdio"],
          "filetypes": ["elm"],
          "rootPatterns": ["elm.json"],
          "initializationOptions": {
            "elmPath": "elm",
            "elmFormatPath": "elm-format",
            "elmTestPath": "elm-test"
          }
        }
      },
      "codeLens.enable": true,
      "vimlsp.suggest.fromVimruntime": true,
      "vimlsp.suggest.fromRuntimepath": true
    }
  '';
}
