{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    systems.url = "github:nix-systems/default";
  };
  outputs = { self, nixpkgs, systems }:
    let
      mkHaskellPackages = pkgs: pkgs.haskellPackages.extend (pkgs.haskell.lib.compose.packageSourceOverrides {
        palytte = self;
      });
      forAllSystems = nixpkgs.lib.genAttrs (import systems);
      forAllPkgs = f: forAllSystems (system: f nixpkgs.legacyPackages.${system});
      forAllHaskellPackages' = f: forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          haskellPackages = mkHaskellPackages pkgs;
        in
        f pkgs haskellPackages
      );
      forAllHaskellPackages = f: forAllHaskellPackages' (pkgs: haskellPackages: f haskellPackages);
    in
    {
      devShells = forAllHaskellPackages' (pkgs: haskellPackages: {
        default = haskellPackages.shellFor {
          packages = ps: [ ps.palytte ];
          withHoogle = true;
          buildInputs = with pkgs; [ hlint just ormolu ];
        };
      });

      packages = forAllHaskellPackages (hs: {
        inherit (hs) palytte;
        default = hs.palytte;
      });

      formatter = forAllPkgs (pkgs: pkgs.nixpkgs-fmt);

      overlays = rec {
        palytte = _final: prev:
          let
            haskellPackages = mkHaskellPackages prev;
          in
          {
            inherit (haskellPackages) palytte;
          };
        default = palytte;
      };
    };
}
