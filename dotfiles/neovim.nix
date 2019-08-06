{ pkgs, ... }:

let plugins = import ./neovim/plugins.nix { inherit pkgs; };
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

        color delek

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
      '';

      packages.myVimPackage = {
        start = with plugins; [
          tpope.vim-sensible
          tpope.vim-surround

          # fzf
          pkgs.fzf
          junegunn."fzf.vim"

          # language server
          neoclide."coc.nvim"

          # filetypes
          LnL7.vim-nix
        ];
        opt = with pkgs.vimPlugins; [ ];
      };
    };
  };
}
