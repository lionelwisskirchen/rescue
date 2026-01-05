{
  config,
  lib,
  inputs,
  pkgs,
  ...
}:
with lib;
let
in
{
  programs.emacs = {
    extraPackages =
      epkgs: with epkgs; [
        ox-reveal
      ];
  };
  home.file = {
    ".emacs.d/custom.d/org-reveal.el".text = ''
      (use-package "ox-reveal"
          :custom
          (load-library "ox-reveal")    
      )
    '';
  };
}
