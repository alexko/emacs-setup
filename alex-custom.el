(when window-system (color-theme-inkpot))
(defun f-toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column 1))))

(global-set-key [f1] 'f-toggle-selective-display)

(defun insert-unixtime ()
  (interactive)
  (insert (format-time-string "%Y%m%d%H%M%S" (current-time))))

(define-key global-map [(control c) (d)] 'insert-date)
(define-key global-map [(control c) (t)] 'insert-unixtime)

(setq abbrev-file-name "~/.emacs.d/.abbrev_defs")
;; (setq save-abbrevs t)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-attach-directory (concat org-directory "/data"))
(setq org-agenda-files (list org-directory))
(setq org-refile-targets '((org-agenda-files . (:level . 1))))
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler)) ;!!
(add-hook 'remember-mode-hook 'org-remember-apply-template)
;; (setq org-remember-templates
;;       '(("Todo" ?t "* TODO %?\n  %i\n  %a" "~/org/todo.org" "Tasks")
;;         ("Journal" ?j "* %U %?\n\n  %i\n  %a" "~/org/journal.org")
;;         ("Idea" ?i "* %?\n  %i\n  %a" "~/org/journal.org" "Ideas")))
(setq org-remember-templates
      (append
       (if (boundp 'org-remember-templates) org-remember-templates nil)
       '(("Todo" ?t "* TODO %^{Title} %^g\n%?\n  %i\n  %a\nAdded: %U" "~/org/tasks.org" "Tasks")
        ("Journal"   ?j "* %^{Title} %U %^g\n%?\n  %i\n  %a" "~/org/journal.org")
        ("Clip" ?x "* %^{Title} %U :xclip:\n%?\n  %x\n  %a" "~/org/journal.org")
        ("Clip" ?y "* %^{Title} %U :yclip:\n%?\n  %c\n  %a" "~/org/journal.org")
        ("Expenses"   ?e "* %^{Title} %U %^g\n%?"   "~/org/finance.org")
        ("Book" ?b "* %^{Title} %t :book: \n%[~/.emacs.d/org/.book.tmpl]\n" 
         "~/org/journal.org")
        ("Review" ?a "* Daily review %T :review: \n%[~/.emacs.d/org/.review.tmpl]\n"
         "~/org/journal.org")
        ("Idea" ?i "* %^{Title} %^g\n%?\n  %i\n  %x\n  %a\nAdded: %U"
         "~/org/journal.org" "Ideas"))))

(define-key global-map "\C-cr" 'org-remember)
;(define-key global-map "\C-cc" 'org-capture)

(setq org-log-done t)
(setq org-odd-levels-only t)
(setq org-hide-leading-stars t)
(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks t)
(setq org-agenda-include-diary t)
(setq org-clock-persist 'history)
;; (org-clock-persistence-insiduate)

(setq org-agenda-custom-commands
      '(("h" "Daily habits" 
         ((agenda ""))
         ((org-agenda-show-log t)
          (org-agenda-ndays 7)
          (org-agenda-log-mode-items '(state))
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))
        ;; other commands here
        ))

(require 'org-learn)
(require 'command-frequency)
(command-frequency-table-load)
(command-frequency-mode 1)
(command-frequency-autosave-mode 1)

;; this pushes id of current entry into the kill ring (crreates id if needed)
;; if invoked with C-u it creates a TRIGGER for the previously pushed id
;; if invoked with C-u C-u it sets the previously pushed id as a BLOCKER
;; see org-depend for explanation of the TRIGGER and BLOCKER properties
(require 'org-depend)
(defun org-make-dependency (arg)
  (interactive
   (if (= (prefix-numeric-value current-prefix-arg) 1)
       (list
        (format "ID %s" (kill-new (org-id-get (point) t)))) ;; push item id on the kill ring
     (let*
         ((id (current-kill 0)) ;; pop item id from the kill ring
          (pos (point))
          (tup
           (if (= (prefix-numeric-value current-prefix-arg) 4)
               (cons "TRIGGER"
                     (format "%s(%s)" id
                             (read-from-minibuffer "TRIGGER STATUS: " "TODO")))
             (cons "BLOCKER" id))))
       (org-entry-add-to-multivalued-property pos (car tup) (cdr tup))
       (list (format "%s = %s" (car tup) (cdr tup))))))
  (message arg))
(add-hook 'org-mode-hook
          '(lambda () (define-key org-mode-map [f12] 'org-make-dependency)))

;; save/restore desktop sessions
;;(load "desktop")
(desktop-load-default)
;;(desktop-read) ;;-> spurious warning: file appears to be used by own pid
(desktop-save-mode 1)
;;(load "pivotal-tracker")
(setq magit-item-highlight
      '((((class color) (background dark)) (:background "gray10"))))
(setq shell-prompt-pattern " ") ;; this prevents tramp from hanging on /sudo::
(provide 'alex-custom)
