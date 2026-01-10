{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager"; # Points to master/unstable
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      ...
    }@inputs:
    let
      inherit (self) outputs;
      systems = [
        "aarch64linux"
        "i686linux"
        "x86_64linux"
        "aarch64darwin"
        "x86_64darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (system: import ./pkgs nixpkgs.legacyPackages.${system});

      homeConfigurations."rescue" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages."x86_64-linux";
        extraSpecialArgs = {
          inherit inputs;
        };
        modules = [
          ./modules/emacs.nix
          ./modules/org.nix
          ./modules/org-agenda.nix
          ./modules/org-roam.nix
          ./modules/presentations.nix    
          ./modules/scientific.nix
          {

            home.stateVersion = "26.05";
            home.username = "rescue";
            home.homeDirectory = "/home/rescue";
            programs.home-manager.enable = true;
          }
        ];
      };
    };
}
