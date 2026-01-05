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

  home.file = {
    ".emacs.d/custom.d/icons/book.svg".source = inputs.self + "/config/icons/book.svg";
    ".emacs.d/custom.d/icons/home.svg".source = inputs.self + "/config/icons/home.svg";
    ".emacs.d/custom.d/icons/solution.svg".source = inputs.self + "/config/icons/solution.svg";
    ".emacs.d/custom.d/icons/work.svg".source = inputs.self + "/config/icons/work.svg";
  };

  home.file = {
    ".emacs.d/custom.d/org-agenda.el".text = ''
      (let ((org-dir "~/org"))
           (unless (file-directory-p org-dir)
           	     (make-directory org-dir t))
      	    )
      (setq org-agenda-files (directory-files-recursively "~/org" "\.org$"))	    

      (customize-set-value
       'org-agenda-category-icon-alist
       `(
         ("Work" "~/.emacs.d/custom.d/icons/work.svg" nil nil :ascent center :mask heuristic)
         ("Projects" "~/.emacs.d/custom.d/icons/solution.svg" nil nil :ascent center :mask heuristic)
         ("Private" "~/.emacs.d/custom.d/icons/home.svg" nil nil :ascent center :mask heuristic)
         ("Uni" "~/.emacs.d/custom.d/icons/book.svg" nil nil :ascent center :mask heuristic)   
         ))
       (setq 	org-agenda-format-date 'my-org-agenda-format-date-aligned
       	org-deadline-warning-days 7
      	org-agenda-breadcrumbs-separator " ❱ "
      	org-agenda-block-separator (string-to-char " "))
      ;; Nice view of the data
      (defun my-org-agenda-format-date-aligned (date)
        "Format a DATE string for display in the daily/weekly agenda, or timeline.
      This function makes sure that dates are aligned for easy reading."
        (require 'cal-iso)
        (let* ((dayname (calendar-day-name date 1 nil))
               (day (cadr date))
               (day-of-week (calendar-day-of-week date))
               (month (car date))
               (monthname (calendar-month-name month 1))
               (year (nth 2 date))
               (iso-week (org-days-to-iso-week
                          (calendar-absolute-from-gregorian date)))
               (weekyear (cond ((and (= month 1) (>= iso-week 52))
                                (1- year))
                               ((and (= month 12) (<= iso-week 1))
                                (1+ year))
                               (t year)))
               (weekstring (if (= day-of-week 1)
                               (format " W%02d" iso-week)
                             "")))
          (format " %-2s. %2d %s"
                  dayname day monthname)))


      ;; Custom agendas
      (setq org-agenda-custom-commands
            '(
              ("a" "My Agenda"
               (
                (agenda "" (
                            (org-agenda-skip-scheduled-if-done nil)
                            (org-agenda-skip-timestamp-if-done t)
                            (org-agenda-skip-deadline-if-done t)
                            (org-agenda-start-day "+0d")
                            (org-agenda-span 4)
                            (org-agenda-overriding-header "⚡ Calendar")
                            (org-agenda-repeating-timestamp-show-all nil)
                            (org-agenda-remove-tags t)
                            (org-agenda-todo-keyword-format "")
                            (org-agenda-time)
                            (org-agenda-current-time-string "→ Now ← ")
      		      (org-agenda-prefix-format "  %t %i %?b ")
                            (org-agenda-scheduled-leaders '("" ""))
                            (org-agenda-deadline-leaders '("Deadline:  " "In %3d d.: " "%2d d. ago: "))))


      	  (tags "+TODO=\"STARTED\"" (
                              (org-agenda-overriding-header "\n⚡ Work in Progress")
                              (org-agenda-sorting-strategy '(priority-down))
                              (org-agenda-remove-tags t)
                              (org-agenda-prefix-format "   %-2i %?b")
                              (org-agenda-todo-keyword-format "")))
      	  
      	  (todo "TODO" (
                              (org-agenda-overriding-header "⚡ To Do")
                              (org-agenda-sorting-strategy '(priority-down))
                              (org-agenda-remove-tags t)
                              ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
      			(org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
      ;;                        (org-agenda-todo-ignore-scheduled 'all)
      			(org-agenda-prefix-format "   %-2i %?b")
                              (org-agenda-todo-keyword-format "")))
      	  
      	  (tags "+CATEGORY=\"Work\"+TODO=\"WAITING\"" (
                              (org-agenda-overriding-header "⚡ On hold")
                              (org-agenda-sorting-strategy '(priority-down))
                              (org-agenda-remove-tags t)
                              (org-agenda-prefix-format "   %-2i %?b")
                              (org-agenda-todo-keyword-format "")))

      	  
      	  (todo "DONE" (
      			(org-agenda-overriding-header "-----------------------------------------------------------\n⚡ Done")
      			(org-agenda-remove-tags t)
                              (org-agenda-prefix-format "   %-2i ")			
      			(org-agenda-todo-keyword-format "")))
                ))
              ))
      (global-set-key (kbd "C-c a") 'org-agenda)

    '';
  };
}
