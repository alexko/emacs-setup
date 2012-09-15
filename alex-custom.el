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
(require 'ob-C)
(require 'ob-octave)
(org-babel-lob-ingest "~/.emacs.d/org/contrib/babel/library-of-babel.org")
(org-babel-lob-ingest "~/org/lob.org")
;;(add-to-list 'org-src-lang-modes (cons "cu" 'c))
(add-to-list 'auto-mode-alist (cons "\\.cu$" 'c++-mode))

(load "ledger")

(when window-system (color-theme-inkpot))
(defun f-toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (if selective-display nil
     (or column (+ 1 (current-column))))))

(global-set-key [f1] 'f-toggle-selective-display)

(defun insert-unixtime ()
  (interactive)
  (insert (format-time-string "%Y%m%d%H%M%S" (current-time))))

(define-key global-map [(control c) (d)] 'insert-date)
(define-key global-map [(control c) (t)] 'insert-unixtime)

(setq ffip-patterns '("*.c", "*.h", "*.cc", "*.cpp", "*.cu",
                      "*.py", "*.el", "*.java", "*.js"))
(put 'ffip-patterns 'safe-local-variable 'listp) ;; fixes ffip setting
;; (defmacro setl (sym val)
;;   "Like setq, but makes sym a local variable first."
;;   `(set (make-local-variable ',sym) ,val))
;; (add-hook 'c++-mode-hook
;;           (lambda () (setl ffip-patterns
;;                       '("*.c", "*.h", "*.cc", "*.cpp", "*.cu",
;;                         "*.py", "*.el", "*.java", "*.js"))))

;; (setq abbrev-file-name "~/.emacs.d/.abbrev_defs")
;; (setq save-abbrevs t)
(setq org-link-abbrev-alist
      '(("g"        . "http://www.google.com/search?q=")
        ("gs"       . "https://encrypted.google.com/search?q=")
        ("gfin"     . "http://www.google.com/finance?q=")
        ("gmap"     . "http://maps.google.com/maps?q=")
        ("nsf"      . "http://nsf.gov/awardsearch/showAward.do?AwardNumber=")
        ("tw"       . "http://twitter.com/")
        ("d"        . "http://www.duckduckgo.com/?q=")
        ("amz"      . "http://www.amazon.com/s/ref=nb_sb_noss_2?url=search-alias%3Daps&field-keywords=")
        ("sd"       . "http://slickdeals.net/newsearch.php?firstonly=1&q=")
        ("cb"       . "http://crunchbase.com/search?query=")
        ("ups"      . "http://wwwapps.ups.com/WebTracking/processRequest?HTMLVersion=5.0&Requester=NES&AgreeToTermsAndConditions=yes&loc=en_US&tracknum=")
        ("ontrack"  . "http://www.ontrac.com/trackres.asp?tracking_number=")
        ("wp"       . "http://en.wikipedia.org/w/index.php?title=Special:Search&search=")
        ("yelp"     . "http://www.yelp.com/search?find_desc=")))

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
(setq org-reverse-note-order t) ;; refiling puts item at the top
(setq org-completion-use-ido t)

(setq org-attach-directory (concat org-directory "/data"))
(setq org-agenda-files (list org-directory))

(setq org-sort-agenda-noeffort-is-high nil)
(setq org-global-properties
      '(("Effort_ALL". "1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00 9:00 0:30")
        ("COLUMNS". "%30ITEM %8TAGS %PRIORITY %TODO %5Effort{:} %5CLOCKSUM")))

(define-key global-map "\C-cb" 'org-ido-switchb)
(global-set-key (kbd "C-<f11>") 'org-clock-in)


(define-key global-map "\C-cc" 'org-capture)
;; (setq org-capture-templates nil)
(setq org-capture-templates
      (append
       (if (boundp 'org-capture-templates) org-capture-templates nil)
       '( ("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
           "* TODO %^{Title} %^g\nAdded: %U\n  %a\n  %i\n%?\n- - -" :prepend t)
          ("j" "Journal" entry (file "~/org/journal.org")
           "* %^{Title} %U %^g\n  %a\n  %i\n%?\n- - -" :prepend t)
          ("x" "Clip" entry (file "~/org/journal.org")
           "* %^{Title} %U :xclip:\n  %a\n  %x\n%?\n- - -" :prepend t)
          ("y" "Clip" entry (file "~/org/journal.org")
           "* %^{Title} %U :yclip:\n  %a\n  %c\n%?\n- - -" :prepend t)
          ("e" "Expenses" entry (file "~/org/finance.org")
           "* %^{Title} %U %^g\n%?\n")
          ("b" "Book" entry (file "~/org/journal.org")
           "* %^{Title} %t :book:\n%[~/.emacs.d/org/.book.tmpl]\n" :prepend t)
          ("a" "Review" entry (file "~/org/journal.org")
           "* Daily review %T :review:\n%[~/.emacs.d/org/.review.tmpl]\n" :prepend t)
          ("i" "Idea" entry (file+headline "~/org/journal.org" "Ideas")
           "* %^{Title} %^g\nAdded: %U\n  %a\n  %i\n  %x\n%?\n- - -"))))

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
;; (setq shell-prompt-pattern " ") ;; this prevents tramp from hanging on /sudo::
(setq shell-prompt-pattern "[^\n]*\\([>#$%][ ]+\\)+$")

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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
(defun my-open-link (k) (org-occur-in-agenda-files k) t)
(add-hook 'org-open-link-functions 'my-open-link)
(define-key occur-mode-map (kbd "q") 'delete-window)

(setq org-task-sample-time nil)

(defun org-task-sample (&optional match)
  "Random sampling of todos from the org file"
  (interactive)
  (let ((rpos
         (save-excursion
           (outline-up-heading 1)
           (skip-chars-forward "*") ;; to get level of the parent entry
           (let* ((level (current-column))
                  (extract
                   (lambda () (cons (point) (org-entry-get nil "Effort" t))))
                  (match (format "+TODO=\"TODO\"+LEVEL=%d" (+ 1 level)))
                  (scope 'tree)
                  (candidates
                   (org-map-entries extract match scope))
                  (pick (random (length candidates))))
             (car (nth pick candidates))))))
    (when (and rpos (/= rpos (point)))
      (push-mark) (goto-char rpos))))

(define-key org-mode-map [f11] 'org-task-sample)

(defun org-toggle-eval-confirmation ()
  (interactive)
  (let ((state (if org-confirm-babel-evaluate nil t)))
    (setq org-confirm-babel-evaluate state
          org-confirm-shell-link-function state
          org-confirm-elisp-link-function state)
    (message
     (concat "org eval confirmation is " (if state "on" "off")))))

(defun adb-org-mobile-sync ()
  "syncs with org-mobile android app via adb"
  (interactive)
  (let ((adb  (expand-file-name "~/android/android-sdk-linux/platform-tools/adb"))
        (org-mobile-remote-dir "/sdcard/org")
        (org-mobile-local-dir (expand-file-name org-mobile-directory)))
    (org-mobile-pull) ;; to prevent overwriting mobileorg.org
    (call-process adb nil "*adb*" nil "-d" "pull"
                  (concat org-mobile-remote-dir "/mobileorg.org")
                  org-mobile-local-dir)
    (org-mobile-pull)
    (org-mobile-push)
    (call-process adb nil "*adb*" nil "-d" "push"
                  org-mobile-local-dir org-mobile-remote-dir)))

(defun my-calc-eval ()
  "calculates expression at the point using calc"
  (interactive)
  (set-mark (point))
  (skip-chars-backward "^ ")
  (exchange-point-and-mark)
  (let ((selection (buffer-substring-no-properties (mark) (point))))
    (insert (concat " = " (calc-eval selection)))))

(global-set-key (kbd "C-=") 'my-calc-eval)

(require 'org-learn)
(require 'command-frequency)
(command-frequency-table-load)
(command-frequency-mode 1)
(command-frequency-autosave-mode 1)
(provide 'alex-custom)
