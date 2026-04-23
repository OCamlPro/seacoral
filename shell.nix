{ pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/master.tar.gz";
  }) {}
}:


let
  ocamlPackages = pkgs.ocamlPackages;

  goblint-cil = ocamlPackages.buildDunePackage (finalAttrs: {
    pname = "goblint-cil";
    version = "2.0.6";

    src = pkgs.fetchurl {
      url = "https://github.com/goblint/cil/releases/download/${finalAttrs.version}/goblint-cil-${finalAttrs.version}.tbz";
      hash = "sha256-VXcAe/rGPD8GCavbdBGf5nTJvIUp15DpDvc6hZZKoHo=";
    };

    buildInputs = with ocamlPackages; [
      dune-configurator
    ];

    nativeBuildInputs = with ocamlPackages; [
      cppo
      dune_3
      ocaml
    ];

    propagatedBuildInputs = with ocamlPackages; [
      findlib
      ppx_deriving_yojson
      pkgs.perl
      zarith
    ];
  });

in

pkgs.mkShell {
  dontDetectOcamlConflicts = true;
  buildInputs = with pkgs.ocamlPackages; [
    cppo
    crunch
    dune_3
    dune-glob
    findlib
    menhir
    ocaml
    ppx_assert
    ppx_expect
    ppx_inline_test
    pkgs.llvmPackages.clang-unwrapped
  ];
  nativeBuildInputs = with pkgs.ocamlPackages; [
    cmdliner
    ctypes
    fmt
    goblint-cil
    frama-c
    frama-c-lannotate
    inotify
    json-data-encoding
    logs
    lwt
    terminal_size
    toml
    yojson
    pkgs.llvmPackages.clang-unwrapped
    pkgs.framac
  ];
}
