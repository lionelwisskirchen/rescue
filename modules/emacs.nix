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
  home.packages = with pkgs; [
    direnv
  ];
    programs.emacs = {
      enable = true;
      extraPackages =
        epkgs: with epkgs; [
          direnv
          use-package
          doom-themes
          nerd-icons
          which-key
          doom-modeline
          sr-speedbar
          hydra
          multi-term
          nix-mode
          yasnippet
          windresize
          which-key
          pdf-tools
        ];
    };

    home.file = {
      ".emacs.d/early-init.el".text = ''
        (scroll-bar-mode -1)        ; Disable visible scrollbar
        (tool-bar-mode -1)          ; Disable the toolbar
        (tooltip-mode -1)           ; Disable tooltips
        (set-fringe-mode 10)        ; Give some breathing room
        (menu-bar-mode -1)            ; Disable the menu bar

        ;; Set up the visible bell
        (setq visible-bell t)

        (column-number-mode)

        (global-display-line-numbers-mode t)
        ;; Disable line numbers for some modes

        (dolist (mode '(org-mode-hook
        		org-agenda-mode
                        term-mode-hook
                        shell-mode-hook
                        treemacs-mode-hook
                        eshell-mode-hook
        		pdf-view-mode-hook))
          (add-hook mode (lambda () (display-line-numbers-mode -1))))

        ;; Misc settings

        ;; Sensible line breaking
        (add-hook 'text-mode-hook 'visual-line-mode)
        ;; Overwrite selected text
        (delete-selection-mode t)
        ;; Only y/n answers
        (defalias 'yes-or-no-p 'y-or-n-p)
        ;; Increase line spacing (not working in terminal mode)
        (setq-default line-spacing 6)

        ;; Basic UI Settings
        (setq inhibit-startup-message t)
      '';

      ".emacs.d/init.el".text = ''

        ;; Load all el files from custom.d
        (let ((custom-dir (expand-file-name "~/.emacs.d/custom.d")))
          (when (file-directory-p custom-dir)
            (dolist (file (directory-files custom-dir t "\\.el$"))
              (load file)))
          )

        (setq desktop-path '("~/"))
        (desktop-save-mode 1)

        (use-package nix-mode
          :mode "\\.nix\\'")

        (use-package direnv
          :ensure t
          :config
          (direnv-mode))

        (use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

        (use-package yasnippet
          :config
          (yas-global-mode t)
          )
        ;; Keep folder clean
        (make-directory "~/.emacs-saves/" t)
        (setq auto-save-file-name-transforms '((".*"  "~/.emacs-saves/" t)))
        (setq lock-file-name-transforms '((".*" "~/.emacs-saves/" t)))
        (setq backup-directory-alist '((".*" . "~/.emacs-saves")))

        (load-theme 'doom-dracula t)
        (doom-modeline-mode 1)
        (setq doom-modeline-time t)
        (display-time-mode t)

        ;; Icons for Doom Modeline
        (defun font-available-p (font-name)
          (find-font (font-spec :name font-name)))

        ;; Speedbar
        ;;Regular expression matching directories not to show in the speedbar
        (setq speedbar-directory-unshown-regexp "^\\(\\.\\.*$\\)\\'")
        ;; Show files of all types
        (setq speedbar-show-unknown-files t)
        ;; Trim large directories names
        (setq speedbar-directory-button-trim-method 'trim)
        ;; Set fixed width size
        (setq sr-speedbar-width 30)
        (setq sr-speedbar-max-width 30)
        ;; Show speedbar at the left side
        (setq sr-speedbar-right-side nil)


        ;; Text Scaling
        (use-package hydra
          :defer t)

        (defhydra hydra-text-scale (:timeout 4)
          "scale text"
          ("j" text-scale-increase "in")
          ("k" text-scale-decrease "out")
          ("f" nil "finished" :exit t))

        ;; Keybindings
        (global-set-key (kbd "C-x e") 'hydra-text-scale/body)
        (global-set-key (kbd "M-)") 'speedbar-refresh)
        (global-set-key (kbd "M-0") 'sr-speedbar-toggle)
        (global-set-key (kbd "C-x t") 'multi-term)
        (global-set-key (kbd "C-c r") 'replace-string)

        (use-package windresize
          :bind (("C-c w" . windresize)))

        ;; Move between buffers
        (use-package windmove
          :bind*
          (("C-c <left>" . windmove-left)
           ("C-c <right>" . windmove-right)
           ("C-c <up>" . windmove-up)
           ("C-c <down>" . windmove-down))
          )

        ;; Which Key
        (use-package which-key
          :defer 0
          :diminish which-key-mode
          :config
          (which-key-mode)
          (setq which-key-idle-delay 1))


        (add-hook 'pdf-view-mode (lambda () (display-line-numbers-mode -1)))

        (use-package pdf-tools
          :init
          (pdf-tools-install)
          :bind (:map pdf-view-mode-map
                      ("e" . pdf-view-revert-buffer)))
      '';
    };
}
