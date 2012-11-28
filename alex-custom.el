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

(color-theme-inkpot)
(defun f-toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (if selective-display nil
     (or column (+ 1 (current-column))))))

(global-set-key [f1] 'f-toggle-selective-display)

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
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets '((nil :maxlevel . 4) ;; current buffer
                           (org-agenda-files :maxlevel . 4)))
; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t) 
; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)
; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Save all org buffers every hour
(run-at-time "00:59" 3600 'org-save-all-org-buffers)

(setq org-attach-directory (concat org-directory "/data"))
(setq org-agenda-files (list org-directory))

(setq org-sort-agenda-noeffort-is-high nil)
(setq org-global-properties
      '(("Effort_ALL". "1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00 9:00 0:30")
        ("COLUMNS". "%30ITEM %PRIORITY %DEADLINE %SCHEDULED %5Effort{:} %5CLOCKSUM")))
(setq calendar-latitude 38)
(setq calendar-longitude -122)

(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-ido-switchb)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(define-key global-map (kbd "C-x C-b") 'ibuffer)
(define-key global-map (kbd "C-x M-f") 'ido-find-file-other-window)
(define-key global-map (kbd "C-x C-M-f") 'find-file-in-project)
(define-key global-map (kbd "C-x g") 'magit-status)
(define-key global-map (kbd "M-g") 'goto-line)
(define-key global-map (kbd "M-/") 'hippie-expand)
(define-key global-map (kbd "<C-f9>") 'compile)
(define-key global-map (kbd "<f9>") 'next-error)

(define-key global-map (kbd "C-x f") 'recentf-ido-find-file)
(define-key global-map (kbd "C-x C-i") 'ido-imenu)

;; (setq org-capture-templates nil)
(setq org-capture-templates
      (append
       (if (boundp 'org-capture-templates) org-capture-templates nil)
       '( ("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
           "* TODO %^{Title} %^g\nAdded: %U\n  %a\n  %i\n%?\n- - -"
           :prepend t :clock-in t :clock-resume t)
          ("j" "Journal" entry (file "~/org/journal.org")
           "* %^{Title} %^g\nAdded: %U\n  %a\n  %i\n%?\n- - -"
           :prepend t :clock-in t :clock-resume t)
          ("x" "Clip" entry (file "~/org/journal.org")
           "* %^{Title} :xclip:\nAdded: %U\n  %a\n  %x\n%?\n- - -"
           :prepend t :clock-in t :clock-resume t)
          ("y" "Clip" entry (file "~/org/journal.org")
           "* %^{Title} :yclip:\nAdded: %U\n  %a\n  %c\n%?\n- - -"
           :prepend t :clock-in t :clock-resume t)
          ("f" "Flagged.org" entry (file "~/org/flagged.org")
           "* %^{Title}\nAdded: %U\n  %a\n  %x\n%?\n- - -"
           :prepend t :clock-in t :clock-resume t)
          ("e" "Expenses" entry (file "~/org/finance.org")
           "* %^{Title} %U %^g\n%?\n"
           :prepend t :clock-in t :clock-resume t)
          ("b" "Book" entry (file "~/org/journal.org")
           "* %^{Title} %t :book:\n%[~/.emacs.d/org/.book.tmpl]\n"
           :prepend t :clock-in t :clock-resume t)
          ("a" "Review" entry (file "~/org/journal.org")
           "* Daily review %T :review:\n%[~/.emacs.d/org/.review.tmpl]\n"
           :prepend t :clock-in t :clock-resume t)
          ("i" "Idea" entry (file+headline "~/org/journal.org" "Ideas")
           "* %^{Title} %^g\nAdded: %U\n  %a\n  %i\n  %x\n%?\n- - -"
           :prepend t :clock-in t :clock-resume t))))

(setq org-log-done t)
(setq org-odd-levels-only nil) ;; org.mobile gets confused if it is enabled
(setq org-hide-leading-stars t)
(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks t)
(setq org-agenda-include-diary t)

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
(defun my-clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in, except capture tasks"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond ((member (org-get-todo-state) (list "TODO")) "NEXT"))))

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

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; save/restore desktop sessions
;;(load "desktop")
(desktop-load-default)
;;(desktop-read) ;;-> spurious warning: file appears to be used by own pid
(desktop-save-mode 1)

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
(global-set-key (kbd "<kp-up>")     'buf-move-up)
(global-set-key (kbd "<kp-down>")   'buf-move-down)
(global-set-key (kbd "<kp-left>")   'buf-move-left)
(global-set-key (kbd "<kp-right>")  'buf-move-right)

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
			(mode . css-mode)))
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

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "pink" :weight bold)
              ("NEXT" :foreground "deepskyblue" :weight bold)
              ("DONE" :foreground "palegreen" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(defun fix-org-column-font ()
  (when (fboundp 'set-face-attribute)
    (set-face-attribute 'org-column nil
                        :height (face-attribute 'default :height)
                        :family (face-attribute 'default :family)
                        :background "grey40")))
(defun org-columns-with-font-fix ()
  (interactive) (fix-org-column-font) (org-columns))
(define-key org-mode-map (kbd "C-c C-x C-c") 'org-columns-with-font-fix)

(set-face-underline-p 'org-link t) ;; not underlined by default since org 7.5
(defun my-open-link (k) (org-occur-in-agenda-files k) t)
(add-hook 'org-open-link-functions 'my-open-link)
(add-to-list 'org-link-frame-setup '(file . find-file-other-frame))
(define-key occur-mode-map (kbd "q") 'delete-window)

(global-set-key (kbd "C-c o") 'occur)
(defun occur-mode-goto-occurrence (&optional event)
  "Go to the occurrence the current line describes."
  (interactive (list last-nonmenu-event))
  (let ((pos
         (if (null event)
             ;; Actually `event-end' works correctly with a nil argument as
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

(defun recent-dl (n)
  "insert link to a recently downloaded file"
  (interactive "p")
  (let ((rfiles
         (split-string (shell-command-to-string "ls -t ~/dl/") "\n")))
    (dotimes (i n)
      (insert (concat "[[~/dl/" (nth i rfiles) "]] ")))))

(defun convert-win-to-frame ()
  "makes frame out of window"
  (interactive)
  (save-excursion
    (make-frame-command)
    (delete-window)))

(define-key global-map (kbd "M-n") 'convert-win-to-frame)
(define-key global-map (kbd "C-c n") nil)

(require 'org-learn)
(require 'command-frequency)
(command-frequency-table-load)
(command-frequency-mode 1)
(command-frequency-autosave-mode 1)
(provide 'alex-custom)
