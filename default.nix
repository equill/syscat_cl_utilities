with import <nixpkgs> {};

stdenv.mkDerivation rec {
    name = "syscat_cl_utils";

    buildInputs = [
        # General utilities
        pkgs.bash
        # Lisp env
        pkgs.libressl
        pkgs.sbcl
    ];

    env = buildEnv {
        name = name;
        paths = buildInputs;
    };

    LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
        pkgs.libressl
    ];

    shellHook = "export PS1='\n\\[\\033[01;32m\\][nix syscat_cl_utils] \\w\\$\\[\\033[00m\\] '";
}
