{ mkDerivation, base, comonad, stdenv }:
mkDerivation {
  pname = "cellular-automata";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base comonad ];
  description = "generic types for cellular automata experiments";
  license = stdenv.lib.licenses.gpl3;
}
