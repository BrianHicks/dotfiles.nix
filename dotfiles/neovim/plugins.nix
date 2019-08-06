{ pkgs, ... }:

{

  LnL7.vim-nix = pkgs.vimUtils.buildVimPlugin {
    name = "vim-nix";
    src = pkgs.fetchFromGitHub {
      owner = "LnL7";
      repo = "vim-nix";
      rev = "a3eed01f4de995a51dfdd06287e44fcb231f6adf";
      sha256 = "0pwdfwws1dj3705m00ghw3dvym5zbm00bfsj023gmbp6vr8wn6yi";
    };
  };

  junegunn."fzf.vim" = pkgs.vimUtils.buildVimPlugin {
    name = "fzf.vim";
    src = pkgs.fetchFromGitHub {
      owner = "junegunn";
      repo = "fzf.vim";
      rev = "359a80e3a34aacbd5257713b6a88aa085337166f";
      sha256 = "0a01g0gw59m57zizidvm9n89qh8lvj0aq4kyxy27i72ysclp3wsf";
    };
  };

  tpope.vim-sensible = pkgs.vimUtils.buildVimPlugin {
    name = "vim-sensible";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-sensible";
      rev = "67fe033b2b56b6f631a4c7a1179865178665f2a4";
      sha256 = "1jhj88n0xj6s6xjx5zs5906y6wwzr855wczk3f5myzs8z8y5cih5";
    };
  };

  tpope.vim-surround = pkgs.vimUtils.buildVimPlugin {
    name = "vim-surround";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-surround";
      rev = "fab8621670f71637e9960003af28365129b1dfd0";
      sha256 = "0lkc0isv1cqv34qfia9mjvnp1nzz0qqy3k47z8r3xzb7dxgymkw8";
    };
  };

}
