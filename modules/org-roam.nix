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
          org-roam
          org-roam-ui
        ];
    };

    home.packages = with pkgs; [

    ];

    home.file = {
      ".emacs.d/custom.d/org-roam.el".text = ''
        (let ((org-dir "~/org/roam"))
          (unless (file-directory-p org-dir)
            (make-directory org-dir t))
          )
        (use-package org-roam
          :ensure t
          :init
          (setq org-roam-v2-ack t)
          :custom
          (org-roam-directory "~/org/roam")
          (org-roam-completion-everywhere t)
          :bind (("C-c n l" . org-roam-buffer-toggle)
                 ("C-c n f" . org-roam-node-find)
                 ("C-c n i" . org-roam-node-insert)
                 :map org-mode-map
                 ("C-M-i" . completion-at-point))
          ;;  :bind-keymap
          ;;  ("C-c n d" . org-roam-dailies-map)
          :config
          ;;  (require 'org-roam-dailies) ;; Ensure the keymap is available
          (org-roam-setup)
          (org-roam-db-autosync-mode))

;; Graphical Frontend for OrgRoam
(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t)
  :bind
  (("C-c o g" . org-roam-ui-open)))

;; Allow space in minibuffer
(define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)
      '';
    };
}
