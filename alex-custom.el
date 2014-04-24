;; org-mode
(add-to-list 'load-path "~/.emacs.d/vendor/use-package")
(require 'use-package)
(add-to-list 'load-path "~/.emacs.d/org/lisp")
(add-to-list 'load-path "~/.emacs.d/org/contrib/lisp")
(use-package org)
(use-package org-install
  :init
  (progn
    (org-babel-do-load-languages
     'org-babel-load-languages
     (quote ((emacs-lisp . t)
             (python . t)
             (ruby . t)
             (lua . t)
             (sh . t)
             (C . t)
             (R . t)
             (js . t)
             (octave . t)
             (latex . t)
             (gnuplot . t)
             (dot . t)
             (ditaa . t)
             (ledger . t))))
    (setq org-babel-results-keyword "results")
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

    ;;(org-babel-lob-ingest "~/.emacs.d/org/contrib/babel/library-of-babel.org")
    (org-babel-lob-ingest "~/org/lob.org")
    (setq org-src-fontify-natively t)
    (dolist (x org-structure-template-alist)
      (setf (cadr x) (downcase (cadr x)))) ;; make them lowercase
    (setq org-src-preserve-indentation nil)
    (setq org-edit-src-content-indentation 0)
    (add-to-list 'org-src-lang-modes (cons "cu" 'c++))
    (add-to-list 'org-src-lang-modes (cons "js" 'espresso))
    (setq org-export-coding-system 'utf-8)

    (setq org-format-latex-options
          (plist-put org-format-latex-options :scale 2.0))

    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|"
                            "CANCELLED(c@/!)" "MEETING(m@/!)"))))

    (setq org-todo-keyword-faces
          (quote (("NEXT" :foreground "gold" :weight bold)
                  ("TODO" :foreground "pink" :weight bold)
                  ("DONE" :foreground "palegreen" :weight bold)
                  ("WAITING" :foreground "deepskyblue" :weight bold)
                  ("HOLD" :foreground "steel blue" :weight bold)
                  ("CANCELLED" :foreground "forest green" :weight bold)
                  ("PHONE" :foreground "forest green" :weight bold))))

    (setq org-link-abbrev-alist
          '(("g"        . "https://www.google.com/search?q=")
            ("gs"       . "http://scholar.google.com/scholar?hl=en&q=")
            ("gb"       . "https://www.google.com/search?tbm=bks&q=")
            ("gt"       . "http://translate.google.com/#auto/en/")
            ("pat"      . "https://www.google.com/search?tbm=pts&q=")
            ("gfin"     . "http://www.google.com/finance?q=")
            ("gmap"     . "http://maps.google.com/maps?q=")
            ("nsf"      . "http://nsf.gov/awardsearch/showAward.do?AwardNumber=")
            ("tw"       . "http://twitter.com/")
            ("d"        . "http://www.duckduckgo.com/?q=")
            ("amz"      . "http://www.amazon.com/s/ref=nb_sb_noss_2?url=search-alias%3Daps&field-keywords=")
            ("sd"       . "http://slickdeals.net/newsearch.php?firstonly=1&q=")
            ("cb"       . "http://crunchbase.com/search?query=")
            ("usc"      . "http://www.law.cornell.edu/uscode/text/")
            ("cfr"      . "http://www.law.cornell.edu/cfr/text/")
            ("ups"      . "http://wwwapps.ups.com/WebTracking/processRequest?HTMLVersion=5.0&Requester=NES&AgreeToTermsAndConditions=yes&loc=en_US&tracknum=")
            ("ontrack"  . "http://www.ontrac.com/trackres.asp?tracking_number=")
            ("wp"       . "http://en.wikipedia.org/w/index.php?title=Special:Search&search=")
            ("yelp"     . "http://www.yelp.com/search?find_desc=")))

    (setq org-directory "~/org")
    (setq org-agenda-files
          '("~/org/notes.org"
            "~/org/tasks.org"))
                                        ;(setq org-agenda-files (list org-directory))
    (setq org-agenda-text-search-extra-files
          '("~/org/journal.org"
            "~/org/bookmarks.org"))
    (setq org-mobile-directory "~/org.mobile")
    (setq org-mobile-files
          (append org-agenda-files org-agenda-text-search-extra-files))
    (setq org-default-notes-file (concat org-directory "/notes.org"))
    (setq org-attach-directory (concat org-directory "/data"))
    (setq org-drawers-for-agenda nil)
    (setq org-mobile-inbox-for-pull "~/org/flagged.org")

    (setq org-file-apps
          '((auto-mode . emacs)
            ("\\.mm\\'" . default)
            ("\\.x?html?\\'" . default)
            ("\\.pdf\\'" . default)
            ("\\.epub\\'" . "fbreader %s")))

    (setq org-refile-use-outline-path (quote file))
    (setq org-reverse-note-order t) ;; refiling puts item at the top
    (setq org-completion-use-ido t)
    (setq org-refile-targets '((nil :maxlevel . 4) ;; current buffer
                               (org-mobile-files :maxlevel . 4)))
                                        ; Use full outline paths for refile targets - we file directly with IDO
    (setq org-refile-use-outline-path 'file)
                                        ; Targets complete directly with IDO
    (setq org-outline-path-complete-in-steps nil)
                                        ; Allow refile to create parent tasks with confirmation
    (setq org-refile-allow-creating-parent-nodes (quote confirm))
    (defun verify-refile-target ()
      "Exclude todo keywords with a done state from refile targets"
      (not (member (nth 2 (org-heading-components)) org-done-keywords)))
    (setq org-refile-target-verify-function 'verify-refile-target)

                                        ; Save all org buffers every hour
    (run-at-time "00:59" 3600 'org-save-all-org-buffers)

    (setq org-sort-agenda-noeffort-is-high nil)
    (setq org-global-properties
          '(("Effort_ALL". "1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00 9:00 0:30")
            ("COLUMNS". "%30ITEM %PRIORITY %DEADLINE %SCHEDULED %5Effort{:} %5CLOCKSUM")))
    ;; (setq org-global-properties nil)
    (setq org-clock-idle-time 10)
    (setq calendar-latitude 38)
    (setq calendar-longitude -122)

    (setq org-special-ctrl-a/e t)
    (setq org-special-ctrl-k t)
    (setq org-yank-adjusted-subtrees t)

    (setq org-log-done t)
    ;;(setq org-log-done (quote time))
    (setq org-log-into-drawer t)
    ;;(setq org-log-state-notes-insert-after-drawers nil)
    (setq org-odd-levels-only nil) ;; org.mobile gets confused if it is enabled
    (setq org-hide-leading-stars t)
    (setq org-enforce-todo-dependencies t)
    (setq org-treat-S-cursor-todo-selection-as-state-change nil)
    (setq org-agenda-include-diary t)

    (add-to-list 'Info-default-directory-list "~/.emacs.d/org/doc")
    (setq org-list-allow-alphabetical t)
    (setq org-cycle-include-plain-lists t)
    (setq org-catch-invisible-edits 'error)
    (setq org-read-date-prefer-future 'time)
    (setq org-agenda-persistent-filter t)
    ;; (setq org-tags-match-list-sublevels 'indented)

    (org-clock-persistence-insinuate)
    (setq org-clock-history-length 36)
    (setq org-clock-in-resume t)
    (setq org-clock-in-switch-to-state 'my-clock-in-to-next)
    (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
    (setq org-clock-into-drawer t)
    (setq org-clock-out-when-done t)
    (setq org-clock-persist t)
    (setq org-clock-persist-query-resume nil)
    (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
    (setq org-clock-report-include-clocking-task t)

    (setq org-stuck-projects
          '("+LEVEL=2/!-DONE-CANCELLED" ("TODO" "NEXT") nil ""))

    (setq org-agenda-dim-blocked-tasks nil)
    (setq org-agenda-compact-blocks t)
                                        ; (setq org-agenda-custom-commands nil) ;; reset
    (setq org-agenda-custom-commands
          (append
           (and (boundp 'org-agenda-custom-commands) org-agenda-custom-commands)
           '(("n" "Next Tasks" tags-todo "-WAITING/!NEXT" ;; ??
              ((org-agenda-overriding-header "Next Tasks")
               (org-agenda-todo-ignore-scheduled t)
               (org-agenda-todo-ignore-deadlines t)
               (org-agenda-todo-ignore-with-date t)
               (org-tags-match-list-sublevels t)
               (org-agenda-sorting-strategy
                '(priority-down effort-up category-keep))) "~/org/next.html")
             ("r" "Entries to refile" tags "REFILE|refile")
             ("w" "Waiting Tasks" todo "WAITING"
              ((org-tags-match-list-sublevels nil)))
             ("h" "Hold Tasks" todo "HOLD")
             ("c" "Calendar" agenda ""
              ((org-agenda-ndays 7)
               (org-agenda-start-on-weekday 0)
               (org-agenda-time-grid nil)
               (org-agenda-repeating-timestamp-show-all t)
               (org-agenda-entry-types '(:timestamp :sexp))))
             ("d" "Upcoming deadlines" agenda ""
              ((org-agenda-time-grid nil)
               (org-deadline-warning-days 365)
               (org-agenda-entry-types '(:deadline))))
             ("Q" . "Custom queries")
             ("Qa" "Search agenda archives" search ""
              ((org-agenda-text-search-extra-files '(agenda-archives))))
             ("Qs" "All .org files search" search ""
              ((org-agenda-files (file-expand-wildcards "~/org/*.org"))))
             ("Q/" "All .org files occur" occur ""
              ((org-agenda-files (file-expand-wildcards "~/org/*.org"))))
             ("Qb" "Bookmarks search" search ""
              ((org-agenda-files '("~/org/bookmarks.org")))) )))

    (setq timestamp-entries t)
    (defun toggle-timestamp-entries ()
      (interactive)
      (message
       (if (setq timestamp-entries (not timestamp-entries)) "on" "off")))
    (defun timestamp-entry ()
      (when timestamp-entries
        (save-excursion
          (org-return)
          (org-cycle)
          (org-insert-time-stamp nil t t))))
    (add-hook 'org-insert-heading-hook 'timestamp-entry 'append)

    (defun my-clock-in-to-next (kw)
      "Switch a task from TODO to NEXT when clocking in, except capture tasks"
      (my-x11idle-set)
      (when (not (and (boundp 'org-capture-mode) org-capture-mode))
        (cond ((member (org-get-todo-state) (list "TODO")) "NEXT"))))

    (defun my-x11idle-set ()
      (setq org-x11idle-exists-p
            (and (eq window-system 'x)
                 (eq (call-process-shell-command
                      "command" nil nil nil "-v" org-clock-x11idle-program-name) 0)
                 (eq (call-process-shell-command
                      org-clock-x11idle-program-name nil nil nil) 0))))

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
      (let ((adb
             (expand-file-name "~/android/android-sdk-linux/platform-tools/adb"))
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

    (defun fix-org-column ()
      (interactive)
      (when (fboundp 'set-face-attribute)
        (let ((family (face-attribute 'default :family)))
          (set-face-attribute 'org-column nil :family family
                              :height 'unspecified)
          (set-face-attribute 'header-line nil :family family
                              :weight 'normal)
          (set-face-foreground 'org-level-2
                               (face-attribute 'default :foreground)))))
    (defun org-columns-with-fix ()
      (interactive) (fix-org-column) (org-columns))
    (define-key org-mode-map (kbd "C-c C-x C-c") 'org-columns-with-fix)

    (defun my-open-link (k)
      (unless (equal (string-to-char k) ?*) (org-occur-in-agenda-files k) t))
    (add-hook 'org-open-link-functions 'my-open-link)
    (add-to-list 'org-link-frame-setup '(file . find-file-other-frame)))

  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-ido-switchb)
         ("C-c l" . org-store-link)))

(use-package org-capture
  :init
  (progn
    ;; (setq org-capture-templates nil)
    (setq org-capture-templates
          (append
           (if (boundp 'org-capture-templates) org-capture-templates nil)
           '( ("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
               "* TODO %^{Title} :%^{Tags|notag}:\n  %a\n  %i\n%?"
               :prepend t :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+headline "~/org/journal.org" "Entries")
               "* %^{Title} :%^{Tags|notag}:\n  %a\n  %i\n%?"
               :prepend t :clock-in t :clock-resume t)
              ("m" "Meeting" entry (file "~/org/journal.org" "Entries")
               "* %^{Title} :%^{Tags|notag}:\n  %a\n  %i\n%?"
               :prepend t :clock-in t :clock-resume t)
              ("x" "Clip" entry (file+headline "~/org/journal.org" "Entries")
               "* %^{Title} :xclip:\n  %a\n %x\n%?"
               :prepend t :clock-in t :clock-resume t)
              ("y" "Clip" entry (file+headline "~/org/journal.org" "Entries")
               "* %^{Title} :yclip:\n  %a\n  %c\n%?"
               :prepend t :clock-in t :clock-resume t)
              ("w" "org-protocol tag" entry (file "~/org/bookmarks.org")
               "* %:description :%^{Tags|notag}:\n  %i\n\n  %:link\n%?"
               :prepend t :clock-in t :clock-resume t)
              ("u" "org-protocol imm" entry (file "~/org/bookmarks.org")
               "* %:description\n  %i\n\n  %:link"
               :prepend t :clock-in t :clock-resume t :immediate-finish t)
              ("c" "org-protocol clk" entry (clock)
               "* %:description :url:\n  %i\n\n  %:link"
               :prepend t :immediate-finish t)
              ("e" "Expenses" entry (file "~/org/finance.org")
               "* %^{Title} %^g\n  %?"
               :prepend t :clock-in t :clock-resume t)
              ("a" "Review" entry (file "~/org/journal.org")
               "* Daily review :review:\n  %[~/.emacs.d/org/.review.tmpl]"
               :prepend t :clock-in t :clock-resume t) )))

    (defadvice org-capture-finalize (after delete-capture-frame activate)
      "Advise capture-finalize to close the frame if it is the capture frame"
      (my-delete-capture-frame))
    (defadvice org-capture-destroy (after delete-capture-frame activate)
      "Advise capture-destroy to close the frame if it is the rememeber frame"
      (my-delete-capture-frame))
    (defun my-delete-capture-frame ()
      (if (equal (frame-parameter nil 'name) "* url capture *")
          (delete-frame)))

    ;; override: add empty line after capture w/o adding a line before
    (defun org-capture-empty-lines-after (&optional n)
      "Add an empty line after capture"
      (org-back-over-empty-lines)
      (while (looking-at "[ \t]*\n") (replace-match ""))
      (save-excursion (if (org-capture-get :prepend) (newline)))))

  :bind ("C-c c" . org-capture))

(use-package org-protocol
  (progn
    (defun org-protocol-do-capture (info &optional capture-func)
      "Support `org-capture' and `org-remember' alike.
CAPTURE-FUNC is either the symbol `org-remember' or `org-capture'."
      (print (org-protocol-split-data info t))
      (let* ((cfunc (or capture-func 'org-capture))
             (parts
              (if (boundp 'org-protocol-data-separatorxxx)
                  (org-protocol-split-data info t org-protocol-data-separator)
                (org-protocol-split-data info t)))
             (template (or (and (>= 2 (length (car parts))) (pop parts))
                           org-protocol-default-template-key))
             (url (org-protocol-sanitize-uri (car parts)))
             (type (if (string-match "^\\([a-z]+\\):" url)
                       (match-string 1 url)))
             (title (or (cadr parts) ""))
             (region (or (caddr parts) ""))
             (orglink (org-make-link-string
                       url (if (string-match "[^[:space:]]" title) title url)))
             (query
              (or
               (and (boundp 'org-protocol-convert-query-to-plist)
                    (org-protocol-convert-query-to-plist (cadddr parts))) ""))
             (org-capture-link-is-already-stored t) ;; avoid call to org-store-link
             remember-annotation-functions)
        ;; (setq org-stored-links
        ;;       (cons (list url title) org-stored-links))
        (kill-new orglink)
        (org-store-link-props :type type
                              :link url
                              :description title
                              :annotation orglink
                              :initial region
                              :query query)
        (if (equal template "w")
            (progn
              (select-frame-set-input-focus
               (make-frame '((name . "* url capture *"))))
              (funcall cfunc nil template)
              (delete-other-windows))
          (funcall cfunc nil template))))))

(use-package org-table)
;;(use-package 'org-collector)

;; this pushes id of current entry into the kill ring (crreates id if needed)
;; if invoked with C-u it creates a TRIGGER for the previously pushed id
;; if invoked with C-u C-u it sets the previously pushed id as a BLOCKER
;; see org-depend for explanation of the TRIGGER and BLOCKER properties
(use-package org-depend
  :init
  (progn
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
    (add-hook 'org-mode-hook 'my-org-mode-hook)))

(use-package org-habit)
(use-package org-learn)
(use-package org-screen)
(use-package org-bookmark)
(use-package org-id
  :init
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))
(use-package org-git-link
  :init ;; prevent interference with normal org linking
  (setq org-store-link-functions
        (delq 'org-git-store-link org-store-link-functions)))
(use-package org-sample
  :init
  (define-key org-mode-map [f11] 'org-task-sample))


(use-package ido
  :init
  (progn
    (setq ido-ignore-directories
          '("\\`CVS/" "\\`\\.\\./" "\\`\\./" "\\`\\.svn" "\\`\\.git"))
    (setq ido-ignore-files
          '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`LICENSE"))
    (setq ido-file-extensions-order
          '(".org" ".py" ".txt" ".el" ".ini" ".cfg" ".cnf"))
    (setq ido-use-filename-at-point 'guess)
    (setq ido-everywhere t)
    (setq ido-max-directory-size 300000)
    (setq ido-default-buffer-method 'selected-window)
    (ido-mode (quote both)))
  :bind (("C-x C-f" . ido-find-file)
         ("C-x f" . recentf-ido-find-file)
         ("C-x M-f" . ido-find-file-other-window)
         ("C-x C-i" . ido-imenu)))

(use-package find-file-in-project
  :init
  (progn
    (setq ffip-patterns '("*.c", "*.h", "*.cc", "*.cpp", "*.cu",
                          "*.py", "*.el", "*.java", "*.js", "*.go"))
    (put 'ffip-patterns 'safe-local-variable 'listp))
  :bind ("C-x C-M-f" . find-file-in-project))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package windmove
  :init
  (progn ;; Make windmove work in org-mode:
    (add-hook 'org-shiftup-final-hook 'windmove-up)
    (add-hook 'org-shiftleft-final-hook 'windmove-left)
    (add-hook 'org-shiftdown-final-hook 'windmove-down)
    (add-hook 'org-shiftright-final-hook 'windmove-right)))

(use-package espresso-mode
  :init
  (progn
    (setq espresso-indent-level 2)
    (add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
    (add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))))

(use-package idle-highlight
  :init (add-hook 'espresso-mode-hook 'idle-highlight))



(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(define-key global-map (kbd "C-x C-b") 'ibuffer)
(define-key global-map (kbd "M-g") 'goto-line)
(define-key global-map (kbd "M-/") 'hippie-expand)
(define-key global-map (kbd "<C-f9>") 'compile)
(define-key global-map (kbd "<f9>") 'next-error)


(set-language-environment "UTF-8")
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(add-to-list 'auto-mode-alist (cons "\\.cu$" 'c++-mode))

(setq fill-column 80
      show-trailing-whitespace t
      remote-shell-program "/usr/bin/ssh"
      compile-command "cd . ; make -j4 -k"
      frame-title-format "%b %+ %[%f%]"
      icon-title-format "%b"
      auto-save-list-file-prefix "~/.emacs.cruft/auto-saves/.saves-"
      backup-directory-alist
      (list (cons "." (expand-file-name "~/.emacs.cruft/backups/")))
      tramp-auto-save-directory "~/.emacs.cruft/auto-saves/"
      tramp-backup-directory-alist '((".*" . "~/.emacs.cruft/backups/")))

;; (setq abbrev-file-name "~/.emacs.d/.abbrev_defs")
;; (setq save-abbrevs t)

(when (file-exists-p "/usr/local/go/misc/emacs/go-mode-load.el")
  (add-to-list 'load-path "/usr/local/go/misc/emacs")
  (require 'go-mode-load))

(load "ledger")

(defun f-toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (if selective-display nil
     (or column (+ 1 (current-column))))))

(global-set-key [f1] 'f-toggle-selective-display)

(defun find-preferred-browser ()
  (let ((candidates '("google-chrome" "chromium-browser" "firefox")))
    (car (delq nil (mapcar 'executable-find candidates)))))
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program (find-preferred-browser))

;; emacsclient opens new frame, closes when done
(add-hook 'server-switch-hook
          (lambda nil
            (let ((server-buf (current-buffer)))
              (bury-buffer)
              (switch-to-buffer-other-frame server-buf))))
(add-hook 'server-done-hook 'delete-frame)

;; save/restore desktop sessions
;;(load "desktop")
;;(desktop-read) ;;-> spurious warning: file appears to be used by own pid
(desktop-save-mode 1)
(add-hook 'desktop-not-loaded-hook (lambda () (desktop-save-mode 0)))
(desktop-load-default)

(use-package tramp
  :init
  (progn
    (add-to-list 'tramp-default-proxies-alist '(nil "root" "/ssh:%h:"))
    (setq shell-prompt-pattern "[^\n]*\\([>#$%][ ]+\\)+$")))

(use-package smex
  :init
  (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

(use-package scim-bridge
  :init
  (progn
    (add-hook 'after-init-hook 'scim-mode-on)
    (scim-define-common-key (kbd "C-`") t)
    ;; Use C-SPC for Set Mark command
    (scim-define-common-key ?\C-\s nil)
    ;; Use C-/ for Undo command
    (scim-define-common-key ?\C-/ nil)
    ;; Change cursor color depending on SCIM status
    (setq scim-cursor-color '("pink" "orange" "limegreen"))))

(use-package color-theme
  :init
  (progn
    (color-theme-inkpot)
    (setq ansi-term-color-vector
          [unspecified "grey40" "red3" "green3" "yellow3"
                       "#6080e0" "#b080d0" "cyan3" "white"])))

(use-package command-frequency
  :idle
  (progn
    (command-frequency-table-load)
    (command-frequency-mode 1)
    (command-frequency-autosave-mode 1)))

(use-package buffer-move
  :bind
  (("<kp-up>"     . buf-move-up)
   ("<kp-down>"   . buf-move-down)
   ("<kp-left>"   . buf-move-left)
   ("<kp-right>"  . buf-move-right)))

(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      '(("home"
         ("Org" (or (mode . org-mode)
                    (filename . "OrgMode")))
         ("Code" (or (mode . python-mode)
                     (mode . c-mode)
                     (mode . lua-mode)))
         ("Web Dev" (or (mode . html-mode)
                        (mode . css-mode)
                        (mode . espresso-mode)))
         ("Emacs-config" (or (filename . ".emacs.d")
                             (filename . "emacs-config")))
         ("Magit" (name . "\*magit"))
         ("ERC" (mode . erc-mode))
         ("Help" (or (name . "\*Help\*")
                     (name . "\*Apropos\*")
                     (name . "\*info\*"))))))
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups "home")))


(define-key occur-mode-map (kbd "q") 'delete-window)

(global-set-key (kbd "C-c o") 'occur)
(defun occur-mode-goto-occurrence (&optional event)
  "Go to the occurrence the current line describes."
  (interactive (list last-nonmenu-event))
  (let ((pos
         (if (null event)
             ;; Actually 'event-end' works correctly with a nil argument as
             ;; well, so we could dispense with this test, but let's not
             ;; rely on this undocumented behavior.
             (occur-mode-find-occurrence)
           (with-current-buffer (window-buffer (posn-window (event-end event)))
             (save-excursion
               (goto-char (posn-point (event-end event)))
               (occur-mode-find-occurrence)))))
        same-window-buffer-names
        same-window-regexps)
    ;;(pop-to-buffer (marker-buffer pos))
    (switch-to-buffer (marker-buffer pos)) ;; stay in the same window
    (goto-char pos)
    (run-hooks 'occur-mode-find-occurrence-hook)))

(defun my-calc-eval ()
  "calculates expression at the point using calc"
  (interactive)
  (set-mark (point))
  (skip-chars-backward "^ \n")
  (exchange-point-and-mark)
  (let* ((selection (buffer-substring-no-properties (mark) (point)))
         (result (calc-eval selection)))
    (insert (concat " = " result))))

(global-set-key (kbd "C-=") 'my-calc-eval)

(defun recent-dl (n)
  "insert link to a recently downloaded file"
  (interactive "p")
  (recent-file n "~/dl/"))

(defun recent-file (n dir)
  "insert link to most recent files in a dir"
  (interactive "p\nD")
  (let ((rfiles
         (split-string (shell-command-to-string
                        (concat "ls -t " dir)) "\n")))
    (dotimes (i n)
      (insert (concat "[[" dir (nth i rfiles) "]] ")))))

(defun remove-blanklines-in-region ()
  "Removes all empty lines in the region"
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (mark))
      (goto-char (point-min))
      (while (search-forward "\n" nil t) (delete-blank-lines)))))

(defun remove-ws-in-region (n)
  "Removes trailing or leading whitespace in the region"
  (interactive "p")
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (mark))
      (goto-char (point-min))
      (while (re-search-forward
              (if (= n 4) "^[ 	]+" "[ 	]+$") nil t)
        (replace-match "" nil nil)))))

(defun remove-newlines-in-region ()
  "Replaces all newlines with spaces"
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (mark))
      (goto-char (point-min))
      (while (search-forward "\n" nil t) (replace-match " " nil t)))))

(defun convert-win-to-frame (arg)
  "makes frame out of window, deletes window with prefix"
  (interactive "P")
  (save-excursion
    (make-frame-command)
    (if arg (delete-window))))

(define-key global-map (kbd "M-n") 'convert-win-to-frame)
(define-key global-map (kbd "C-c n") nil)

(provide 'alex-custom)
