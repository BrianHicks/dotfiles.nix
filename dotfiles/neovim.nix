{ pkgs, ... }:

{
  programs.neovim = {
    enable = true;

    # aliases
    viAlias = true;
    vimAlias = true;

    configure = {
      customRC = ''
        inoremap fd <ESC>
        nnoremap : ;
        nnoremap ; :

        " automatically change working directory when editing a file, so :edit
        " and friends work relatively. Causes problems with :make, which I will
        " fix later.
        nnoremap <leader>cd :lcd %:p:h<CR>

        " automatically make missing directories on write
        augroup Mkdir
          autocmd!
          autocmd BufWritePre *
            \ if !isdirectory(expand("<afile>:p:h")) |
                \ call mkdir(expand("<afile>:p:h"), "p") |
            \ endif
        augroup END
      '';

      # TODO: does this have to be called myVimPackage? Why? Seems to be in
      # `pkgs.wrapNeovim`
      packages.myVimPackage = with pkgs.vimPlugins; {
        start = [
          vim-surround
          vim-sensible
          pkgs.fzf
          "fzf.vim"

          ## LANGUAGES ##
          # nix language support
          vim-nix
        ];
      };
    };
  };
}
