## open emacs until I get symlinking application to a nix-darwin user working

open_emacs() {
    open -a $(dirname $(dirname $(readlink $(which emacs))))/Applications/Emacs.app
}

alias e=open_emacs
