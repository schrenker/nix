{ pkgs ? import <nixpkgs> {} }:


pkgs.emacs29.overrideAttrs (old: {
  pname = "emacs";
    version = "29.1-rc1";
    variant = "mainline";
    src = pkgs.fetchFromSavannah {
      repo = "emacs";
      rev = "29.1-rc1";
      hash = "sha256-p0lBSKsHrFwYTqO5UVIF/PgiqwdhYQE4oUVcPtd+gsU=";
    };
  buildInputs = old.buildInputs ++ [ pkgs.dbus pkgs.mailutils pkgs.imagemagick pkgs.libgccjit pkgs.libtool pkgs.gnutls ];
  configureFlags = old.configureFlags ++ [ "--with-imagemagick" "--with-native-compilation=aot" "--with-poll" "--with-no-frame-refocus" "--with-dbus" "--with-mailutils" ];
  patches =
    (old.patches or [])
    ++ [
      (pkgs.fetchpatch {
        url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/poll.patch";
        sha256 = "1xhri4bz2ifn2mc96ics1zl5sm8isx2ivgzxp35vjriz7ya4rpwc";
      })

      (pkgs.fetchpatch {
        url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
        sha256 = "1hcfm6dxy2ji7q8fw502757920axffy32qlk9pcmpmk6q1zclgzv";
      })

      (pkgs.fetchpatch {
        url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/no-frame-refocus-cocoa.patch";
        sha256 = "1xwgpvgmmqkj9xqarqgp33iim6r4sn32805jh8mcx90idaaakca0";
      })

      (pkgs.fetchpatch {
        url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/round-undecorated-frame.patch";
        sha256 = "1vkp4ybrcdqik1m6syxwixx0n5xcap8k1ak0wvqrlvlz7hsk30mr";
      })

      (pkgs.fetchpatch {
        url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/system-appearance.patch";
        sha256 = "1zxnmvh7kbd5xg245ki97pr6kiv4fihmxwyskh0mfnf2smfrzkm0";
      })

    ];
})
