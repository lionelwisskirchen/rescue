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
          org
          org-bullets
          visual-fill-column
          org-appear
          mixed-pitch
        ];
    };

    home.packages = with pkgs; [
    ];
    home.file = {
      ".emacs.d/custom.d/org.el".text = ''
                        ;; Follow links when hitting enter
                        (setq org-return-follows-link t)
                        ;; Keybindings
                        (global-set-key (kbd "C-x p") 'org-toggle-pretty-entities)

                         (setq org-startup-indented t
                        	org-pretty-entities t
                        	org-hide-emphasis-markers t
                        	org-startup-with-inline-images t
                        	org-image-actual-width '(300)
                        	org-ellipsis " ▾")
                        	
                        (setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
                        (add-hook 'org-mode-hook 'org-bullets-mode)

                        (defun efs/org-mode-visual-fill ()
                          (setq visual-fill-column-width 100
                                visual-fill-column-center-text t)
                          (visual-fill-column-mode 1))
                          
                        (add-hook 'org-mode-hook 'efs/org-mode-visual-fill)
                        (add-hook 'org-mode-hook 'org-appear-mode)


                        ;; Fonts

                        ;; Font Styling
                        (custom-set-faces
                         ;; custom-set-faces was added by Custom.
                         ;; If you edit it by hand, you could mess it up, so be careful.
                         ;; Your init file should contain only one such instance.
                         ;; If there is more than one, they won't work right.
                         '(org-document-title ((t (:inherit default :weight normal :foreground "#f8f8f2" :font "DejaVu Sans Mono" :height 1.5 :underline nil))))
                         '(org-level-1 ((t (:inherit default :weight normal :foreground "#f8f8f2" :font "DejaVu Sans Mono" :weight bold :height 1.2))))
                         '(org-level-2 ((t (:inherit default :weight normal :foreground "#f8f8f2" :font "DejaVu Sans Mono" :height 1.0))))
                         '(org-level-3 ((t (:inherit default :weight normal :foreground "#f8f8f2" :font "DejaVu Sans Mono" :height 1.0))))
                         '(org-level-4 ((t (:inherit default :weight normal :foreground "#f8f8f2" :font "DejaVu Sans Mono"))))
                         '(org-level-5 ((t (:inherit default :weight normal :foreground "#f8f8f2" :font "DejaVu Sans Mono"))))
                         '(org-level-6 ((t (:inherit default :weight normal :foreground "#f8f8f2" :font "DejaVu Sans Mono"))))
                         '(org-level-7 ((t (:inherit default :weight normal :foreground "#f8f8f2" :font "DejaVu Sans Mono"))))
                         '(org-level-8 ((t (:inherit default :weight normal :foreground "#f8f8f2" :font "DejaVu Sans Mono")))))
                        (put 'upcase-region 'disabled nil)

                        (add-hook 'text-mode 'mixed-pitch-mode)
                        (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 120)
                          (set-face-attribute 'fixed-pitch nil :font "DejaVu Sans Mono")
                            (set-face-attribute 'variable-pitch nil :family "DejaVu Sans")


                        (with-eval-after-load 'org
                          (org-babel-do-load-languages
                           'org-babel-load-languages
                           '((ruby . t)
                             (python . t)))

                          (push '("conf-unix" . conf-unix) org-src-lang-modes))


                        (with-eval-after-load 'org
                          ;; This is needed as of Org 9.2
                          (require 'org-tempo)
                          ;; <py will be #+begin_src python
                          (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
                          (add-to-list 'org-structure-template-alist '("rb" . "src ruby"))
                          (add-to-list 'org-structure-template-alist '("py" . "src python")))


        (defun my-org-screenshot ()
          "Take a screenshot into a time stamped unique-named file in the
        same directory as the org-buffer and insert a link to this file."
          (interactive)
          (setq filename
                (concat
                 (make-temp-name
                  (concat (buffer-file-name)
                          "_"
                          (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
          (call-process "spectacle" nil nil nil "-r" "-b" "-n" "-o" filename)
          (insert (concat "[[" filename "]]"))
          (org-display-inline-images))
        (global-set-key (kbd "C-c s") 'my-org-screenshot)
      '';
    };
}
