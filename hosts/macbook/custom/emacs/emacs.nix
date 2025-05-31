{ pkgs ? import <nixpkgs> { } }:

pkgs.emacs.overrideAttrs (old: {
  buildInputs = old.buildInputs
    ++ [ pkgs.mailutils pkgs.imagemagick pkgs.libgccjit ];
  configureFlags = old.configureFlags ++ [
    "--with-imagemagick"
    #"--with-native-compilation=aot"
    "--with-mailutils"
    "--with-xwidgets"
  ];
  patches = (old.patches or [ ]) ++ [
    ./patches/round-undecorated-frame.patch
    ./patches/system-appearance.patch
    ./patches/fix-window-role.patch
  ];
  postInstall = ''
    ${old.postInstall}
    cp ${./Emacs.icns} $out/Applications/Emacs.app/Contents/Resources/Emacs.icns
  '';
})
