{ mkDerivation, base, comonad, stdenv, word8 }:
mkDerivation {
  pname = "cellular-automata";
  version = "0.0.0.0";
  src = /home/octal/cellular-automata;
  buildDepends = [ base comonad word8 ];
  description = "generic types for cullular automata experiments";
  license = stdenv.lib.licenses.gpl3;
}
