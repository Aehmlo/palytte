{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (pkgs) lib;
      haskellPackages = pkgs.haskellPackages.extend (pkgs.haskell.lib.compose.packageSourceOverrides {
        palytte = self;
      });
    in
    {
      devShells.default = haskellPackages.shellFor {
        packages = ps: [ ps.palytte ];
        withHoogle = true;
        buildInputs = with pkgs; [ hlint just ormolu ];
      };
      packages.default = haskellPackages.palytte;
      formatter = pkgs.nixpkgs-fmt;
    });
}
