{ mkDerivation, base, comonad, lens, stdenv, word8 }:
mkDerivation {
  pname = "cellular-automata";
  version = "0.0.0.0";
  src = ./.;
  buildDepends = [ base comonad lens word8 ];
  description = "generic types for cullular automata experiments";
  license = stdenv.lib.licenses.gpl3;
}
