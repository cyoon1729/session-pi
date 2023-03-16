with import <nixpkgs> {};

let
  ocamlPackages = pkgs.recurseIntoAttrs pkgs.ocamlPackages;
  ocamlInit = pkgs.writeText "ocamlinit" ''
    #use "topfind";;
    #thread;;
    #camlp4o;;
    #require "core";;
    #require "async";;
    #require "core.syntax";;
  '';
in
pkgs.mkShell rec {
  buildInputs = with pkgs; [
  ] ++ ( with ocamlPackages;
  [
    ocaml
    dune_3
    core
    core_extended
    async
    findlib
  ]);
  IN_NIX_SHELL = 1;
  OCAMLINIT = "${ocamlInit}";
  shellHook = ''
    alias ocaml="ocaml -init ${ocamlInit}"
  '';
}
