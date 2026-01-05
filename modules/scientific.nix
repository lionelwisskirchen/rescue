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
    enable = true;
    extraPackages =
      epkgs: with epkgs; [
        citar
        citar-org-roam
      ];
  };

  home.packages = with pkgs; [
    zotero
    (pkgs.texlive.combine {
      inherit (pkgs.texlive)
        scheme-medium
        dvisvgm
        dvipng
        wrapfig
        amsmath
        ulem
        hyperref
        capt-of
        ec
        minted
        upquote
        biblatex
        xurl
        ieeetran
        pgfplots
        pgf-pie
        ;
    })
  ];

  home.file = {
    ".emacs.d/custom.d/scientific.el".text = ''
                    ;; Allow space in minibuffer
                    (define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)
                    (add-hook 'doc-view-mode (lambda () (display-line-numbers-mode -1)))
                      
              ;; Make code blocks nice in org export via latex
              (setq org-latex-listings 'minted)
              ;; inside .emacs file
              (setq org-latex-listings 'minted
                    org-latex-packages-alist '(("" "minted") ("dvipsnames" "xcolor"))
                    org-latex-pdf-process
                    '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                      "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                      "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

              (setq org-latex-minted-options
                    '(("linenos" "true") ("bgcolor=white")("bgcolorpadding=0.5em")("numbersep=7pt")("xleftmargin=10pt")("vspace=15pt")("breaklines" "true")("samepage" "true")("breakanywhere" "true")))
              (put 'downcase-region 'disabled nil)

              (with-eval-after-load "ox-latex"
                (add-to-list 'org-latex-classes
                             '("koma-article" "\\documentclass{scrartcl}"
                               ("\\section{%s}" . "\\section*{%s}")
                               ("\\subsection{%s}" . "\\subsection*{%s}")
                               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                               ("\\paragraph{%s}" . "\\paragraph*{%s}")
                               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))                                 
              	)

              )

              (with-eval-after-load "ox-latex"
                (add-to-list 'org-latex-classes
              	       '("IEEEtran" "\\documentclass[conference]{IEEEtran}"
              		 ("\\section{%s}" . "\\section*{%s}")
              		 ("\\subsection{%s}" . "\\subsection*{%s}")
              		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
              		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))	     )

              )

      ;; Bibliography Management
      (use-package citar
        :custom
        (org-cite-global-bibliography
         (directory-files
          (concat (getenv "HOME") "/Library/") t
          "^[A-Z|a-z|0-9].+.bib$"))
        (citar-bibliography org-cite-global-bibliography)
        (org-cite-insert-processor 'citar)
        (org-cite-follow-processor 'citar)
        (org-cite-activate-processor 'citar)

        :bind
        (("C-c d o" . citar-open)
         (:map org-mode-map
               :package org
               ("C-c b" . #'org-cite-insert))))

      (setq warning-suppress-types '((ox-latex)))
      (setq org-latex-caption-above nil)

      ;; Locals dir for csl Export !!!
      (setq org-cite-csl-locales-dir "/home/lionel/locals/")
      (setq org-latex-prefer-user-labels t)
      ;; Bibliography in Orgroam
      (use-package citar-org-roam
              :config (citar-org-roam-mode))
    '';
  };
}
