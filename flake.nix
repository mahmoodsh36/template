{
  description = "my blog generator using cltpt";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    cltpt = {
      url = "github:mahmoodsh36/cltpt";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, cltpt }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        cltpt-lib = cltpt.packages.${system}.cltpt-lib;

        sbcl' = pkgs.sbcl.withOverrides (
          self': super: {
            inherit cltpt-lib;
          }
        );

        template-lib = sbcl'.buildASDFSystem {
          pname = "template";
          version = "dev";
          src = ./.;
          systems = [ "template" ];
          lispLibs = [ cltpt-lib sbcl'.pkgs.cl-json ];
        };

        sbcl = sbcl'.withPackages (p: [ template-lib ]);

        generate-blog = pkgs.writeShellApplication {
          name = "generate-blog";
          runtimeInputs = [ sbcl ];
          text = ''
            export WORK_DIR=''${WORK_DIR:-$(pwd)}
            export BRAIN_DIR=''${BRAIN_DIR:-$WORK_DIR/brain}
            export DATA_DIR=''${DATA_DIR:-$WORK_DIR/data}
            sbcl --script "${self}/generate.lisp"
          '';
        };
      in
      {
        packages.default = generate-blog;
        packages.generate = generate-blog;
        devShells.default = pkgs.mkShell {
          buildInputs = [ sbcl ];
          shellHook = ''
            export WORK_DIR=''${WORK_DIR:-$(pwd)}
            export BRAIN_DIR=''${BRAIN_DIR:-$WORK_DIR/brain}
            export DATA_DIR=''${DATA_DIR:-$WORK_DIR/data}
          '';
        };
      }
    );
}