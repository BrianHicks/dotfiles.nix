{ pkgs, ... }:

let
  plugins = pkgs.callPackage ./neovim/plugins.nix { };

  brian-nur = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "BrianHicks";
    repo = "nur-packages";
    rev = "671f226ad54421832183eaa10c0b71d03dc4d7a3";
    sha256 = "1b2hfrnvsix0j0s3jlhhgqj6pgfgl0bx5j6gy9cr3kgv46ffgggi";
  }) { };
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

        "" FZF ""
        """""""""

        " don't show fzf statusline
        autocmd! FileType fzf
        autocmd  FileType fzf set laststatus=0 noshowmode noruler
          \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

        " bindings for fuzzy-finding
        nnoremap <leader>ff :call fzf#run(fzf#wrap({"source": "${pkgs.python3}/bin/python ${
          ./neovim/similar-sort.py
        } 'git ls-files' " . @%, "sink": "edit"}))<CR>

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
        let g:coc_global_extensions = [ 'coc-git', 'coc-json', 'coc-yaml', 'coc-html', 'coc-vimlsp' ]

        nnoremap <leader>lR :CocRestart<CR>
        nnoremap <leader>la :CocAction<CR>
        nnoremap <leader>lc :CocCommand<CR>
        nnoremap <leader>lf :CocFix<CR>
        nnoremap <leader>li :CocInfo<CR>
        nnoremap <leader>ll :CocList<CR>
        nnoremap <leader>lo :CocOpenLog<CR>
        nnoremap <leader>lr :CocListResume<CR>

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

        " gundo
        nnoremap <leader>u :GundoToggle<CR>
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
