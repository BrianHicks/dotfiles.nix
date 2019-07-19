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
      '';

      # TODO: does this have to be called myVimPackage? Why? Seems to be in
      # `pkgs.wrapNeovim`
      packages.myVimPackage = with pkgs.vimPlugins; {
        start = [ vim-surround ];
      };
    };
  };
}
