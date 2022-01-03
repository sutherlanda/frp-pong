{
  description = "Pong using functional-reactive programming";

  inputs = {
    nixpkgs = { url = github:nixos/nixpkgs/nixpkgs-unstable; };
    flake-utils = { url = github:numtide/flake-utils; };
    nix-projects = { url = github:sutherlanda/nix-projects; };
    #nix-projects = { url = path:/Users/andrewsutherland/projects/nix-projects; };
  };

  outputs = { self, nixpkgs, flake-utils, nix-projects, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let

        rootPath = "$PWD";

        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        haskellPackages = p: [
          p.OpenGL
          p.GLFW-b
        ];

        projectLib = import nix-projects;

        haskellProjectConfigs = [
          rec {
            name = "pong";
            projectRoot = "${rootPath}/${name}";
            srcDir = "${projectRoot}/src";
            executables = {
              main = "Main.hs";
            };
          }
        ];

        haskellLib = projectLib.lib.projects.haskell {
          inherit pkgs haskellPackages;
        };

      in
      { devShell = haskellLib.mkShell haskellProjectConfigs; });
}
