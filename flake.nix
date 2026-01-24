# SPDX-FileCopyrightText: 2025 Łukasz Niemier <~@hauleth.dev>
#
# SPDX-License-Identifier: Apache-2.0

{
  description = "A very basic flake";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.devenv.url = "github:cachix/devenv";
  inputs.systems.url = "github:nix-systems/default";
  inputs.kamid = {
    url = "github:omar-polo/kamid";
    flake = false;
  };

  outputs = {flake-parts, ...} @ inputs:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        inputs.devenv.flakeModule
      ];

      systems = import inputs.systems;

      perSystem = {pkgs, ...}: let
        kamid = pkgs.callPackage ./nix/kamid { inherit inputs; };
      in {
        formatter = pkgs.alejandra;

        packages = {
          inherit kamid;
        };

        devenv.shells.default = {
          languages.erlang.enable = true;

          packages = [ kamid pkgs._9pfs ];

          # env.ERL_FLAGS = ''
          # -kernel logger_level debug
          # -kernel logger '[
          #   {filters, log, [{no_progress, {fun logger_filters:progress/2, stop}}]}
          # ]'
          # -kernel shell_history enabled
          # '';
        };
      };
    };
}
