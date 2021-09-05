# upgrading to 21.05

I recently upgraded my gitea server to 21.05 and it was fine.
Next is my laptop!

But I'd really like to clean up my dotfiles before then.
I wrote all this when I was just learning Nix, nix-darwin, and home-manager, and they feel messier than I'd really like.

I'm going to write down some things that I'd like and try to figure out how realistic they are:

- **Use flakes to control versions of stuff instead of `niv`.**
  Not that there's anything wrong with `niv`, but it seems like flakes will be the thing going forward and I'd like to figure them out.
  It looks like home-manager can already source from flakes, so it may be a matter of enabling them in my config and hoping all the software I use supports building with them.

...

I had other things written here (stop using `nix-darwin`, better organize things in `dotfiles`, simplify my Kakoune configuration) but they're all little independent projects that don't need to be a part of 21.05!
