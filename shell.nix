with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, containers, hashable, lens, llvm-general
             , llvm-general-pure, primitive, QuickCheck, stdenv, stm
             , test-framework, test-framework-quickcheck2, transformers
             , unordered-containers
             }:
             mkDerivation {
               pname = "compiled-expressions";
               version = "0.1.0.0";
               src = ./.;
               buildDepends = [
                 base containers hashable lens llvm-general llvm-general-pure
                 primitive stm transformers unordered-containers
               ];
               testDepends = [
                 base containers QuickCheck test-framework
                 test-framework-quickcheck2
               ];
               description = "Compile a certain class of Haskell expressions to efficient machine code at run-time";
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
