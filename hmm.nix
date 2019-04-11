{ mkDerivation, base, hmatrix, linear, mtl, papa, stdenv
, transformers, vector
}:
mkDerivation {
  pname = "hmm-haskell";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base hmatrix linear mtl papa transformers vector
  ];
  license = stdenv.lib.licenses.asl20;
}
