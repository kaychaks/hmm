{ mkDerivation, base, combinatorial, hmatrix, linear, mtl, papa
, stdenv, transformers, vector
}:
mkDerivation {
  pname = "hmm";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base combinatorial hmatrix linear mtl papa transformers vector
  ];
  license = stdenv.lib.licenses.asl20;
}
