with (import <nixpkgs> {});

  pkgs.mkShell {
    name = "novo-dev";
    hardeningDisable = [ "all" ];
    nativeBuildInputs = with pkgs; [
      clang-tools
      clang
      gdb
      meson
      ninja
      graphviz
    ];
    buildInputs = [];
  }
