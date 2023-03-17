with import <nixpkgs> {};

let
  ocamlPackages = pkgs.recurseIntoAttrs pkgs.ocamlPackages;
  ocamlInit = pkgs.writeText "ocamlinit" ''
    #use "topfind";;
    #thread;;
    #camlp4o;;
    #require "base";;
    #require "core";;
    #require "core_unix";;
    #require "async";;
    #require "core.syntax";;
    #require "ppx_let";;
    #require "thread";;
  '';
in
pkgs.mkShell rec {
  buildInputs = with pkgs; [
  ] ++ ( with ocamlPackages;
  [
    ocaml
    dune_3
    base
    core
    core_unix
    core_extended
    async
    findlib
    ppx_let
  ]);
  IN_NIX_SHELL = 1;
  OCAMLINIT = "${ocamlInit}";
  shellHook = ''
    alias ocaml="ocaml -init ${ocamlInit}"
  '';
}
