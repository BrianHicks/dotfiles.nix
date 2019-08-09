{ pkgs, ... }:

rec {

  ElmCast.elm-vim = pkgs.vimUtils.buildVimPlugin {
    name = "elm-vim";
    src = pkgs.fetchFromGitHub {
      owner = "ElmCast";
      repo = "elm-vim";
      rev = "165107a9fd2b20c8f050fc4f977b4e41c790b1e7";
      sha256 = "0gf7b49by0ybx3ndz7sz5dwcfnps4sz6wsr02lyarj8f8116ysy5";
    };
  };

  LnL7.vim-nix = pkgs.vimUtils.buildVimPlugin {
    name = "vim-nix";
    src = pkgs.fetchFromGitHub {
      owner = "LnL7";
      repo = "vim-nix";
      rev = "a3eed01f4de995a51dfdd06287e44fcb231f6adf";
      sha256 = "0pwdfwws1dj3705m00ghw3dvym5zbm00bfsj023gmbp6vr8wn6yi";
    };
  };

  itchyny."lightline.vim" = pkgs.vimUtils.buildVimPlugin {
    name = "lightline.vim";
    src = pkgs.fetchFromGitHub {
      owner = "itchyny";
      repo = "lightline.vim";
      rev = "f5039419d87b76accee7000319b394ce25a0dbfb";
      sha256 = "0cfjw1jpddw92jz62ly8m6waxknj19cazff01x8drk1lr9xj6wdy";
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

  neoclide."coc.nvim" = pkgs.vimUtils.buildVimPlugin {
    name = "coc.nvim";
    src = pkgs.fetchFromGitHub {
      owner = "neoclide";
      repo = "coc.nvim";
      rev = "v0.0.73";
      sha256 = "1z7573rbh806nmkh75hr1kbhxr4jysv6k9x01fcyjfwricpa3cf7";
    };
  };

  rakr.vim-one = pkgs.vimUtils.buildVimPlugin {
    name = "vim-one";
    src = pkgs.fetchFromGitHub {
      owner = "rakr";
      repo = "vim-one";
      rev = "6695e135415c53a8fbe96672e382200aa1ffb4b4";
      sha256 = "0951r9kw23zlwd7fda6ib2d9k0akpfd2hvh82pasgw3ks9v1n2vf";
    };
  };

  tpope.vim-fugitive = pkgs.vimUtils.buildVimPlugin {
    name = "vim-fugitive";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-fugitive";
      rev = "bc3d36e559e446d963e0b1dd832d3b39ea233241";
      sha256 = "1072ldcvp1kq5kckzxvr6g6qijf2a6ccrig1v63jydpdqmn8h6kq";
    };
  };

  tpope.vim-repeat = pkgs.vimUtils.buildVimPlugin {
    name = "vim-repeat";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-repeat";
      rev = "ae361bea990e27d5beade3a8d9fa22e25cec3100";
      sha256 = "0myqas20r81gfvfrph4ww38db1nz32qlph7syr6ym8zl7c1fcjd4";
    };
  };

  tpope.vim-sensible = pkgs.vimUtils.buildVimPlugin {
    name = "vim-sensible";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-sensible";
      rev = "c176d137892f33945d3d4dd766fd21611e9b5ddf";
      sha256 = "11adqaccwph4z5a4kyycd1gbc1l9np4za0d4fbd3cnh1zqf2xzjz";
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

  tpope.vim-unimpaired = pkgs.vimUtils.buildVimPlugin {
    name = "vim-unimpaired";
    src = pkgs.fetchFromGitHub {
      owner = "tpope";
      repo = "vim-unimpaired";
      rev = "ab7082c0e89df594a5ba111e18af17b3377d216d";
      sha256 = "1gvzjihkxnc84kd7sdh26kmm0rqi19xmwiisfqhf307yqyqa6lkj";
    };
  };

  all = [
    ElmCast.elm-vim
    LnL7.vim-nix
    itchyny."lightline.vim"
    junegunn."fzf.vim"
    neoclide."coc.nvim"
    rakr.vim-one
    tpope.vim-fugitive
    tpope.vim-repeat
    tpope.vim-sensible
    tpope.vim-surround
    tpope.vim-unimpaired
  ];
}
