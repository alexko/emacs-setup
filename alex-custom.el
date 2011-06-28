;; org-mode
(setq load-path (cons "~/.emacs.d/org/lisp" load-path))
(setq load-path (cons "~/.emacs.d/org/contrib/lisp" load-path))
(require 'org-install)
(require 'ob-python)
(require 'ob-emacs-lisp)
(require 'ob-gnuplot)
(require 'ob-ruby)
(require 'ob-sh)
(require 'ob-R)
(require 'ob-ledger)
(require 'ob-latex)
(require 'ob-lua)
(org-babel-lob-ingest "~/org/lob.org")

(load "ledger")

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

;; (setq abbrev-file-name "~/.emacs.d/.abbrev_defs")
;; (setq save-abbrevs t)
(setq org-link-abbrev-alist
      '(("g"        . "http://www.google.com/search?q=")
        ("gs"       . "https://encrypted.google.com/search?q=")
        ("gfin"     . "http://www.google.com/finance?q=")
        ("gmap"     . "http://maps.google.com/maps?q=%s")
        ("nsf"      . "http://nsf.gov/awardsearch/showAward.do?AwardNumber=")
        ("tw"       . "http://twitter.com/")))

(defun find-preferred-browser ()
  (let ((candidates (list
                     "google-chrome"
                     "chromium-browser"
                     "firefox")))
    (car (delq nil (mapcar 'executable-find candidates)))))
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program (find-preferred-browser))

(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-mobile-directory "~/org.mobile")
;; the org-mobile-files are org-agenda-files by default
(setq org-mobile-files '("~/org/notes.org"
                         "~/org/tasks.org"
                         "~/org/journal.org"))
(setq org-drawers-for-agenda nil)
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
;;(setq org-mobile-use-encryption t)
;;(setq org-mobile-encryption-passpword "omega52")
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 1))))
(setq org-refile-use-outline-path (quote file))
(setq org-completion-use-ido t)

(setq org-attach-directory (concat org-directory "/data"))
(setq org-agenda-files (list org-directory))
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

(define-key global-map "\C-cb" 'org-ido-switchb)
(global-set-key (kbd "C-<f11>") 'org-clock-in)
(define-key global-map "\C-cr" 'org-remember)


(define-key global-map "\C-cc" 'org-capture)
;; (setq org-capture-templates nil)
(setq org-capture-templates
      (append
       (if (boundp 'org-capture-templates) org-capture-templates nil)
       '( ("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
           "* TODO %^{Title} %^g\n%?\n  %i\n  %a\nAdded: %U" :prepend t)
          ("j" "Journal" entry (file "~/org/journal.org")
           "* %^{Title} %U %^g\n%?\n  %i\n  %a" :prepend t)
          ("x" "Clip" entry (file "~/org/journal.org")
           "* %^{Title} %U :xclip:\n%?\n  %x\n %a" :prepend t)
          ("y" "Clip" entry (file "~/org/journal.org")
           "* %^{Title} %U :yclip:\n%?\n  %c\n  %a" :prepend t)
          ("e" "Expenses" entry (file "~/org/finance.org")
           "* %^{Title} %U %^g\n%?")
          ("b" "Book" entry (file "~/org/journal.org")
           "* %^{Title} %t :book:\n%[~/.emacs.d/org/.book.tmpl]\n" :prepend t)
          ("a" "Review" entry (file "~/org/journal.org")
           "* Daily review %T :review:\n%[~/.emacs.d/org/.review.tmpl]\n" :prepend t)
          ("i" "Idea" entry (file+headline "~/org/journal.org" "Ideas")
           "* %^{Title} %^g\n%?\n  %i\n  %x\n  %a\nAdded: %U"))))

(setq org-log-done t)
(setq org-odd-levels-only nil) ;; org.mobile gets confused if it is enabled
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

(require 'org-collector)

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
(defun my-org-mode-hook ()
  (define-key org-mode-map [f12] 'org-make-dependency)
  (auto-fill-mode 1))
(add-hook 'org-mode-hook 'my-org-mode-hook)

;; save/restore desktop sessions
;;(load "desktop")
(desktop-load-default)
;;(desktop-read) ;;-> spurious warning: file appears to be used by own pid
(desktop-save-mode 1)
;;(load "pivotal-tracker")
(setq magit-item-highlight
      '((((class color) (background dark)) (:background "gray10"))))

(require 'tramp)
(add-to-list 'tramp-default-proxies-alist '(nil "root" "/ssh:%h:"))
(setq shell-prompt-pattern " ") ;; this prevents tramp from hanging on /sudo::

(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(defun fix-org-column () ;; this seems not necessary in org 7.5
  (interactive)
  (custom-set-faces
   '(org-column
     ((t (:family "DejaVu Sans Mono" :height 120))))))

(set-face-underline-p 'org-link t) ;; not underlined by default since org 7.5

(defun org-task-sample () ;; random sampling of todos from the org file
  (interactive)
  (let* ((process
          (lambda () (cons (point) (org-entry-get nil "PRIORITY" t))))
         (match "+TODO=\"TODO\"|+TODO=\"BUG\"|+TODO=\"IDEA\"")
         (scope nil)
         (lst (org-map-entries process match scope))
         (ri (random (length lst)))
         (rp (car (nth ri lst))))
    (push-mark)
    (goto-char rp)))
(global-set-key [f11] 'org-task-sample)

(defun org-toggle-eval-confirmation ()
  (interactive)
  (let ((state (if org-confirm-babel-evaluate nil t)))
    (setq org-confirm-babel-evaluate state
          org-confirm-shell-link-function state
          org-confirm-elisp-link-function state)
    (message
     (concat "org eval confirmation is " (if state "on" "off")))))

(defun adb-org-mobile-sync ()
  (interactive)
  (let ((adb "/home/alkos/android/android-sdk-linux_86/tools/adb")
        (org-mobile-remote-dir "/sdcard")
        (org-mobile-local-dir (expand-file-name org-mobile-directory)))
    (org-mobile-pull) ;; to prevent overwriting mobileorg.org
    (call-process adb nil "*adb*" nil "-d" "pull"
                  (concat org-mobile-remote-dir "/mobileorg.org")
                  org-mobile-local-dir)
    (org-mobile-pull)
    (org-mobile-push)
    (call-process adb nil "*adb*" nil "-d" "push"
                  org-mobile-local-dir org-mobile-remote-dir)))

(require 'org-learn)
(require 'command-frequency)
(command-frequency-table-load)
(command-frequency-mode 1)
(command-frequency-autosave-mode 1)
(provide 'alex-custom)
