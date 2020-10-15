{ haskellLib
, haskellPackages }:
haskellPackages.override {
  overrides = self: super: {
    # Put any overrides here, e.g:
    #
    # Jailbreak a certain package:
    #   pandoc-lens = haskellLib.doJailbreak super.pandoc-lens;
    #
    # Use a local copy of another package:
    #   pandoc-wrapper = super.callPackage ../pandoc-wrapper/pkg.nix { };

    # Use a local checkout of formatting until 7.0 is in nixpkgs
    formatting = super.callPackage ../formatting { };

    # Fix package versions to get co-log working
    co-log = haskellLib.unmarkBroken super.co-log;
    typerep-map = haskellLib.doJailbreak (haskellLib.unmarkBroken super.typerep-map);
  };
}
