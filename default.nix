{ mkDerivation, base, hakyll, lib }:
mkDerivation {
  pname = "website";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hakyll ];
  license = lib.licenses.bsd2;
  mainProgram = "website";
}
