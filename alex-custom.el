;; org-mode
(use-package org-install
  :load-path "org/lisp")
(use-package org
  :load-path "org/contrib/lisp"
  :config
  (progn
    (setq org-babel-results-keyword "results")
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t) (python . t) (ruby . t) (lua . t) (sh . t)
       (C . t) (R . t) (js . t) (octave . t) (ledger . t)
       (latex . t) (gnuplot . t) (dot . t) (ditaa . t)))
    (add-hook 'org-babel-after-execute-hook
              'org-display-inline-images 'append)
    ;; (org-babel-lob-ingest
    ;;   "~/.emacs.d/org/contrib/babel/library-of-babel.org")
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
    ;; (setq org-agenda-files (list org-directory))
    (setq org-agenda-files
          '("~/org/notes.org"
            "~/org/tasks.org"))
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
    (setq org-imenu-depth 3)                       ; default is 2
    (setq org-refile-targets '((nil :maxlevel . 4) ;; current buffer
                               (org-mobile-files :maxlevel . 4)))
    ;; Use full outline paths for refile targets - we file directly with IDO
    (setq org-refile-use-outline-path 'file)
    ;; Targets complete directly with IDO
    (setq org-outline-path-complete-in-steps nil)
    ;; Allow refile to create parent tasks with confirmation
    (setq org-refile-allow-creating-parent-nodes (quote confirm))
    (defun verify-refile-target ()
      "Exclude todo keywords with a done state from refile targets"
      (not (member (nth 2 (org-heading-components)) org-done-keywords)))
    (setq org-refile-target-verify-function 'verify-refile-target)

    ;; Save all org buffers every hour
    (run-at-time "00:59" 3600 'org-save-all-org-buffers)

    (setq org-sort-agenda-noeffort-is-high nil)
    (setq org-global-properties
          '(("Effort_ALL". "1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00 9:00 0:30")
            ("COLUMNS". "%48ITEM %PRIORITY %DEADLINE %SCHEDULED %7Effort{:} %5CLOCKSUM")))
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

    (add-to-list 'Info-default-directory-list
                 (concat dotfiles-dir "/org/doc"))
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
      (unless (boundp 'org-clock-x11idle-program-name)
        (setq org-clock-x11idle-program-name "x11idle"))
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

    (defadvice org-columns (before ak-fix-org-columns activate)
      (when (fboundp 'set-face-attribute)
        (let ((family (face-attribute 'default :family)))
          (set-face-attribute 'org-column nil :family family
                              :height 'unspecified)
          (set-face-attribute 'header-line nil :family family
                              :weight 'normal)
          (set-face-foreground 'org-level-2
                               (face-attribute 'default :foreground)))))

    (defun my-open-link (k)
      (unless (equal (string-to-char k) ?*) (org-occur-in-agenda-files k) t))
    (add-hook 'org-open-link-functions 'my-open-link)
    (add-to-list 'org-link-frame-setup '(file . find-file-other-frame)))

  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-ido-switchb)
         ("C-c l" . org-store-link)))

(use-package org-capture
  :config
  (progn
    ;; (setq org-capture-templates nil)
    (setq org-capture-templates
          (append
           (if (boundp 'org-capture-templates) org-capture-templates nil)
           '( ("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
               "* TODO %^{Title} :%^{Tags|notag}:\n  %a\n  %i\n%?"
               :prepend t :empty-lines-after 1 :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+headline "~/org/journal.org" "Entries")
               "* %^{Title} :%^{Tags|notag}:\n  %a\n  %i\n%?"
               :prepend t :empty-lines-after 1 :clock-in t :clock-resume t)
              ("m" "Meeting" entry (file+headline "~/org/journal.org" "Entries")
               "* %^{Title} :%^{Tags|notag}:\n  %a\n  %i\n%?"
               :prepend t :empty-lines-after 1 :clock-in t :clock-resume t)
              ("x" "Clip" entry (file+headline "~/org/journal.org" "Entries")
               "* %^{Title} :xclip:\n  %a\n %x\n%?"
               :prepend t :empty-lines-after 1 :clock-in t :clock-resume t)
              ("y" "Clip" entry (file+headline "~/org/journal.org" "Entries")
               "* %^{Title} :yclip:\n  %a\n  %c\n%?"
               :prepend t :empty-lines-after 1 :clock-in t :clock-resume t)
              ("w" "org-protocol tag" entry (file "~/org/bookmarks.org")
               "* %:description :%^{Tags|notag}:\n  %i\n\n  %:link\n%?"
               :prepend t :empty-lines-after 1 :clock-in t :clock-resume t)
              ("u" "org-protocol imm" entry (file "~/org/bookmarks.org")
               "* %:description\n  %i\n\n  %:link"
               :prepend t :empty-lines-after 1 :clock-in t :clock-resume t
               :immediate-finish t)
              ("c" "org-protocol clk" entry (clock)
               "* %:description :url:\n  %i\n\n  %:link"
               :prepend t :empty-lines 1 :immediate-finish t)
              ("e" "Expenses" entry (file "~/org/finance.org")
               "* %^{Title} %^g\n  %?"
               :prepend t :empty-lines-after 1 :clock-in t :clock-resume t)
              ("a" "Review" entry (file "~/org/journal.org")
               (concat
                "* Daily review :review:\n  %[.review.tmpl]")
               :prepend t :empty-lines-after 1 :clock-in t :clock-resume t) )))

    ;; this fixes "The mark is not set now, so there is no region" error
    (defadvice org-capture-steal-local-variables (around donot-steal activate))

    (defadvice org-capture-fill-template (around ak-capture-point-fix activate)
      "prevents org-capture-fill-template from moving point"
      (save-excursion ad-do-it))
    (defadvice org-capture-finalize (after delete-capture-frame activate)
      "Advise capture-finalize to close the frame if it is a capture frame"
      (my-delete-capture-frame))
    (defadvice org-capture-destroy (after delete-capture-frame activate)
      "Advise capture-destroy to close the frame if it is a capture frame"
      (my-delete-capture-frame))
    (defun my-delete-capture-frame ()
      (if (equal (frame-parameter nil 'name) "* url capture *")
          (delete-frame))))

  :bind ("C-c c" . org-capture))

;; see README.org for bookmarklets to use with this setup of org-protocol
(use-package org-protocol
  :config
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
        (funcall cfunc nil template)))))

(use-package org-table)
;;(use-package 'org-collector)

;; this pushes id of current entry into the kill ring (crreates id if needed)
;; if invoked with C-u it creates a TRIGGER for the previously pushed id
;; if invoked with C-u C-u it sets the previously pushed id as a BLOCKER
;; see org-depend for explanation of the TRIGGER and BLOCKER properties
(use-package org-depend
  :config
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
(use-package org-player
  :if window-system
  :commands org-player-start/stop
  :init
  (use-package bongo
    :commands bongo))
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
    ;; (setq ido-use-filename-at-point 'guess)
    (setq ido-everywhere t)
    (setq ido-enable-flex-matching t)
    (setq ido-max-directory-size 300000)
    (setq ido-default-buffer-method 'selected-window))
    (ido-mode (quote both))
    (defun ido-recentf-find-file ()
      "Find a recent file using ido."
      (interactive)
      (let ((file (ido-completing-read "Recent files: " recentf-list nil t)))
        (when file (find-file file))))
    (defun ido-execute-extended-command ()
      (interactive)
      (call-interactively
       (intern
        (ido-completing-read
         "M-x "
         (all-completions "" obarray 'commandp)))))
    (define-key global-map (kbd "M-x") 'ido-execute-extended-command)
    (define-key global-map (kbd "C-x f") 'ido-recentf-find-file)
    (define-key global-map (kbd "C-x C-f") 'ido-find-file)
    (define-key global-map (kbd "C-x M-f") 'ido-find-file-other-window))

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
    (setq espresso-indent-level 2)
  :mode
    (("\\.js$" . espresso-mode) ("\\.json$" . espresso-mode)))

(use-package idle-highlight
  :init (add-hook 'espresso-mode-hook 'idle-highlight))

(set-language-environment "UTF-8")
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(add-to-list 'auto-mode-alist (cons "\\.cu$" 'c++-mode))
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)

(setq cruft-dir "~/.emacs.cruft/")
(setq fill-column 80
      show-trailing-whitespace t
      initial-scratch-message ""
      initial-major-mode 'org-mode
      spell-command "aspell"
      visible-bell t
      text-mode-hook '(turn-on-auto-fill text-mode-hook-identify)
      remote-shell-program "/usr/bin/ssh"
      compile-command "cd . ; make -j4 -k"
      frame-title-format "%b %+ %[%f%]"
      icon-title-format "%b"
      auto-save-list-file-prefix (concat cruft-dir "auto-saves/.saves-")
      backup-directory-alist
      (list (cons "." (concat cruft-dir "backups/"))))

(use-package tramp
  :init
  (setq tramp-auto-save-directory (concat cruft-dir "auto-saves/")
        tramp-backup-directory-alist backup-directory-alist
        shell-prompt-pattern "[^\n]*\\([>#$%][ ]+\\)+$")
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "root" "/ssh:%h:")))

;; (setq abbrev-file-name (concat dotfiles-dir ".abbrev_defs"))
;; (setq save-abbrevs t)

;; emacsclient opens new frame, closes when done
(add-hook 'server-switch-hook
          (lambda nil
            (let ((server-buf (current-buffer)))
              (bury-buffer)
              (switch-to-buffer-other-frame server-buf))))
(add-hook 'server-done-hook 'delete-frame)

;; save/restore desktop sessions
;; work around emacs bug#5645 (read-event blocks in batch mode)
(defadvice sit-for
  (around my-sit-for activate)
  "Be non-interactive while starting a daemon."
  (if (and (daemonp) (not (boundp 'server-process)))
           (let ((noninteractive t)) ad-do-it) ad-do-it))

;; this sets after-init-hook
(add-hook 'desktop-not-loaded-hook (lambda () (desktop-save-mode 0)))
(desktop-save-mode 1)

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

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(define-key global-map (kbd "C-x C-b") 'ibuffer)
(define-key global-map (kbd "M-g") 'goto-line)
(define-key global-map (kbd "M-/") 'hippie-expand)
(define-key global-map (kbd "<f5>") 'edebug-defun)
(define-key global-map (kbd "<C-f9>") 'compile)
(define-key global-map (kbd "<f9>") 'next-error)
(define-key global-map (kbd "C-c o") 'occur)
(define-key global-map (kbd "C-h /") 'find-function)
(define-key occur-mode-map (kbd "q") 'delete-window)

(use-package ledger)

(use-package smex
  :config
  (progn
    (smex-initialize)
    (define-key global-map (kbd "M-x") 'smex)
    (define-key global-map (kbd "M-X") 'smex-major-mode-commands)
    (define-key global-map (kbd "C-c C-c M-x") 'execute-extended-command)))

(use-package scim-bridge
  :config
  (progn
    (add-hook 'after-init-hook 'scim-mode-on)
    (scim-define-common-key (kbd "C-`") t)
    ;; Use C-SPC for Set Mark command
    (scim-define-common-key ?\C-\s nil)
    ;; Use C-/ for Undo command
    (scim-define-common-key ?\C-/ nil)))

(use-package color-theme
  :config
  (when (fboundp 'color-theme-lilacs)
    (color-theme-lilacs)))

(use-package command-frequency
  :config
  (progn
    (setq command-frequency-table-file
          (concat cruft-dir ".emacs.frequencies"))
    (command-frequency-table-load)
    (command-frequency-mode 1)
    (command-frequency-autosave-mode 1)))

(use-package buffer-move
  :bind
  (("<kp-up>"     . buf-move-up)
   ("<kp-down>"   . buf-move-down)
   ("<kp-left>"   . buf-move-left)
   ("<kp-right>"  . buf-move-right)))

(use-package ess-site)

(use-package go-mode-load
  :load-path "/usr/local/go/misc/emacs")

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

(defadvice occur-mode-goto-occurrence (around my-occur activate)
  "Open occurrences in the same window if occur window is active"
  (let ((pop-to-buffer-save (symbol-function 'pop-to-buffer)))
    (fset 'pop-to-buffer 'switch-to-buffer)
    ad-do-it
    (fset 'pop-to-buffer pop-to-buffer-save)))

(provide 'alex-custom)
