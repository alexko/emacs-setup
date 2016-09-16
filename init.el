;;; init.el

(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

(set-language-environment "UTF-8")
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(defvar saves-dir (expand-file-name ".saves/" user-emacs-directory))
(make-directory saves-dir t) ;; make sure saves-dir exists
(setq custom-file (concat user-emacs-directory "custom.el")
      backup-directory-alist `((".*" . ,(concat saves-dir "backups/")))
      auto-save-list-file-prefix (concat saves-dir "auto-saves/.saves-")
      ;; auto-save-file-name-transforms `((".*" ,saves-dir t))
      version-control t
      kept-new-versions 5
      delete-old-versions t
      backup-by-copying t)

(setq inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode
      visible-bell t
      echo-keystrokes 0.1
      scroll-preserve-screen-position t
      x-select-enable-clipboard t
      x-select-enable-primary t
      fill-column 80
      text-mode-hook '(turn-on-auto-fill text-mode-hook-identify)
      remote-shell-program "/usr/bin/ssh"
      compile-command "cd . ; make -j4 -k"
      frame-title-format "%b %+ %[%f%]"
      icon-title-format "%b")

(defvar prog-modes
  (list 'c-mode 'lua-mode 'python-mode 'espresso-mode
        'emacs-lisp-mode))
(defvar prog-mode-hooks
  (list 'c-mode-common-hook 'lua-mode-hook 'python-mode-hook
        'espresso-mode-hook 'emacs-lisp-mode-hook))
(dolist (mode prog-modes)
  (font-lock-add-keywords
   mode '(("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend) ; ???
         ("\\<\\(\\?\\?\\?\\)" 1 font-lock-warning-face prepend))))

(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil) ; use spaces
(setq-default tab-width 2)
(setq-default python-indent 2)
;; (setq-default python-guess-indent nil)
(add-to-list 'auto-mode-alist (cons "\\.cu$" 'c++-mode))
(add-to-list 'auto-mode-alist (cons "\\.cl$" 'c++-mode))
(add-to-list 'auto-mode-alist (cons "\\.pyx$" 'python-mode))
(add-to-list 'auto-mode-alist (cons "\\.R$" 'R-mode))
(show-paren-mode 1)
(column-number-mode 1)
;; (size-indication-mode 0)

;; emacsclient opens new frame, closes when done
(add-hook 'server-switch-hook
          (lambda nil
            (let ((server-buf (current-buffer)))
              (bury-buffer)
              (switch-to-buffer-other-frame server-buf))))
(add-hook 'server-done-hook 'delete-frame)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(define-key global-map (kbd "M-g") 'goto-line)
(define-key global-map (kbd "M-/") 'hippie-expand)
(define-key global-map (kbd "<f5>") 'edebug-defun)
(define-key global-map (kbd "C-c o") 'occur)
(define-key global-map (kbd "C-h /") 'find-function)

(let ((default-directory (concat user-emacs-directory "vendor/")))
  (normal-top-level-add-subdirs-to-load-path)) ; this appends load-path

;; (add-to-list 'load-path (concat user-emacs-directory "/elpa-to-submit/"))
(add-to-list 'load-path (concat user-emacs-directory "/vendor/"))
(add-to-list 'load-path (concat user-emacs-directory user-login-name "/"))

;;(regen-autoloads)
;;(load autoload-file)
(load custom-file 'noerror)

(require 'use-package)
(setq use-package-verbose t)
(require 'cl)
(use-package ansi-color)

(use-package package
  :init (setq package-user-dir (concat user-emacs-directory "elpa/")
              package-archives
              '(("melpa" . "http://melpa.milkbox.net/packages/")
               ("gnu" . "http://elpa.gnu.org/packages/")))
  :config (package-initialize))

(use-package ffap
;; :init (ffap-bindings) ; these conflict with ido bindings
 :init (setq ido-use-filename-at-point 'guess))

(use-package uniquify
  "more descriptive buffer names if the file name is the same"
  :init (setq uniquify-buffer-name-style 'forward))

(use-package recentf
  :init
  (setq recentf-save-file (expand-file-name ".recentf" saves-dir)
        recentf-auto-cleanup 'never)
  :config (recentf-mode 1))

(use-package ido
  :init
  (progn
    (setq ido-ignore-directories
          '("\\`CVS/" "\\`\\.\\./" "\\`\\./" "\\`\\.svn" "\\`\\.git"
            "\\`\\.saves"))
    (setq ido-ignore-files
          '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./"
            "\\`LICENSE" "\\`COPYING" "\\`.emacs.desktop" "\\`.emacs-places"
            "\\`.emacs.bmk" "\\`\\.org-id-locations" "\\`org-clock-save.el"
            "\\`.histfile"))
    (setq ido-file-extensions-order
          '(".org" ".py" ".txt" ".el" ".ini" ".cfg" ".cnf"))
    (setq ido-use-filename-at-point 'guess)
    (setq ido-auto-merge-delay-time 1.5)
    (setq ido-everywhere t)
    (setq ido-enable-flex-matching t)
    (setq ido-max-directory-size 300000)
    (setq ido-default-buffer-method 'selected-window))
    (ido-mode (quote both))
    (defun ido-recentf-find-file ()
      "Find a recent file using ido."
      (interactive)
      (let ((file
             (ido-completing-read
              "Recent files: "
              (mapcar 'abbreviate-file-name recentf-list) nil t)))
        (when file (find-file file))))
    (defun ido-execute-extended-command ()
      (interactive)
      (call-interactively
       (intern
        (ido-completing-read
         "M-x "
         (all-completions "" obarray 'commandp)))))
    (defun ido-bookmark-jump (bookmark)
      "*Switch to bookmark using ido."
      (interactive
       (list (ido-completing-read "Bookmark: " (bookmark-all-names) nil t)))
      (if (null current-prefix-arg) (bookmark-jump bookmark)
        (bookmark-jump-other-window bookmark)))

    (define-key global-map (kbd "M-x") 'ido-execute-extended-command)
    (define-key global-map (kbd "C-x f") 'ido-recentf-find-file)
    (define-key global-map (kbd "C-x C-f") 'ido-find-file)
    (define-key global-map (kbd "C-x M-f") 'ido-find-file-other-window)
    (define-key global-map (kbd "C-x r b") 'ido-bookmark-jump))

(defun my-quit-window ()
  (interactive)
  (if (listp (car (window-tree)))
      (delete-window)
    (bury-buffer)))

(use-package "replace"
  :config
  (define-key occur-mode-map (kbd "q") 'my-quit-window))

(use-package compile
  :config
  (progn
    (define-key global-map (kbd "<C-f9>") 'recompile)
    (define-key global-map (kbd "<f9>") 'next-error)
    (define-key global-map (kbd "S-<f9>") 'previous-error)
    (define-key global-map (kbd "C-x ~") 'previous-error)
    (define-key compilation-mode-map (kbd "q") 'my-quit-window)
    (define-key compilation-mode-map (kbd "M-n") nil)))

(use-package grep
  :config
  (progn
    (grep-compute-defaults)
    (define-key grep-mode-map (kbd "q") 'my-quit-window)))

(defvar my-kill-buffer-history nil
  "the history of killed buffers")
(defun my-kill-buffer (arg)
  "kills buffer unless it is open elsewhere, removes its window/frame"
  (interactive "P")
  (if arg
      (let ((fname (car my-kill-buffer-history)))
        (setq my-kill-buffer-history (cdr my-kill-buffer-history))
        (if fname (find-file-other-frame fname)))
    (when
        (= 1 (length (get-buffer-window-list (current-buffer) nil t)))
      (setq my-kill-buffer-history
            (cons buffer-file-name my-kill-buffer-history))
      (kill-buffer (current-buffer)))
    (if (listp (car (window-tree)))
        (delete-window)
      (delete-frame))))
(define-key global-map (kbd "C-x q") 'my-kill-buffer)

(use-package imenu
  :commands imenu--make-index-alist)

(use-package loccur
  :bind (("C-o" . loccur-current)
         ("C-M-o" . loccur)
         ("C-S-o" . loccur-previous-match)))

(use-package find-file-in-project
  :init
  (progn
    (setq ffip-patterns '("*.c" "*.h" "*.cc" "*.cpp" "*.cu"
                          "*.py" ".pyx" "*.R" "*.el" "*.java" "*.js" "*.go"))
    (put 'ffip-patterns 'safe-local-variable 'listp))
  :bind ("C-x C-M-f" . find-file-in-project))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package compile
  :defer t
  :config
  (setq compilation-scroll-output 'first-error))

(use-package diff
  :defer t
  :config
  (setq diff-switches "-u"))

(use-package ediff
  :defer t
  :config
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package windmove
  :init
  (progn ;; Make windmove work in org-mode:
    (add-hook 'org-shiftup-final-hook 'windmove-up)
    (add-hook 'org-shiftleft-final-hook 'windmove-left)
    (add-hook 'org-shiftdown-final-hook 'windmove-down)
    (add-hook 'org-shiftright-final-hook 'windmove-right)))

(use-package js2-mode
  :init
  (setq js2-basic-offset 2
        js2-cleanup-whitespace t)
  :commands js2-mode)

(or
 (use-package espresso
   :init
   (setq espresso-indent-level 2)
   :mode
   (("\\.js$" . espresso-mode) ("\\.json$" . espresso-mode)))

 (use-package generic-x
   :mode
   (("\\.js$" . javascript-generic-mode) ("\\.json$" . javascript-generic-mode))))

(use-package pymacs
  :load-path "vendor/pymacs"
  ;; :requires python ; prevents pymacs from loading
  :config
  (progn
    (require 'python)
    (setq ropemacs-autoimport-modules '("os" "shutil" "random" "bisect"))
    (setq ropemacs-enable-autoimport t)
    (setq ropemacs-enable-shortcuts nil) ;; keep M-/ for hippie-expand
    ;; (setq ropemacs-local-prefix "C-c C-p")
    (pymacs-load "ropemacs" "rope-")
    ;; (define-key python-mode-map (kbd "M-/") 'rope-code-assist)
    (define-key python-mode-map (kbd "C-c g") 'rope-goto-definition)
    (define-key python-mode-map (kbd "C-c u") 'rope-pop-mark)
    (define-key python-mode-map (kbd "C-c d") 'rope-show-doc)
    (define-key python-mode-map (kbd "C-c f") 'rope-find-occurrences)
    (define-key python-mode-map (kbd "M-?") 'rope-lucky-assist))
  :commands (pymacs-eval pymacs-exec pymacs-load ropemacs-mode))

;; begin from tim-custom
(use-package sgml-mode
  :mode (("\\.html\\'" . html-mode) ("\\.mako\\'" . html-mode)))

(use-package nginx-mode
  :init
  (add-to-list 'auto-mode-alist
               '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)))

(use-package mmm-auto
  :load-path "vendor/mmm-mode"
  :config (setq mmm-global-mode 'maybe))

(use-package mmm-mako
  :load-path "vendor/mmm-mako"
  :config
  (when (functionp 'mmm-add-mode-ext-class)
    (mmm-add-mode-ext-class 'html-mode "\\.mako\\'" 'mako)))

(use-package google-c-style
  :config
  (progn
    (add-hook 'c-mode-common-hook 'google-set-c-style)
    (add-hook 'c-mode-common-hook 'google-make-newline-indent)))

(use-package py-indent
  :disabled t
  :init (setq-default py-indent-offset 2))
;; end from tim-custom

(use-package eldoc
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
    (add-hook 'ielm-mode-hook 'eldoc-mode)
    (eldoc-add-command 'paredit-backward-delete
                       'paredit-close-round)))

(use-package rainbow-delimiters
  :config
  (if (boundp 'prog-mode-hook)
      (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    (dolist (hook prog-mode-hooks)
      (add-hook hook 'rainbow-delimiters-mode))))

(use-package idle-highlight
  :init (add-hook 'espresso-mode-hook 'idle-highlight))

(use-package tramp
  :init
  (setq tramp-default-method "ssh"
        shell-prompt-pattern "[^\n]*\\([>#$%][ ]+\\)+$"
        tramp-auto-save-directory (concat saves-dir "auto-saves/")
        tramp-backup-directory-alist backup-directory-alist
        tramp-persistency-file-name (expand-file-name "tramp" saves-dir))
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "root" "/ssh:%h:")))

(use-package shell
  :defer t
  :config
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

(use-package saveplace
  "saves the cursor position for every opem file"
  :init
  (setq save-place-file (concat saves-dir ".emacs-places"))
  (setq-default save-place t))

(use-package dired
  :init
  (setq image-dired-dir (concat saves-dir "image-dired/")
        image-dired-db-file (expand-file-name ".image-dired_db" saves-dir)
        image-dired-temp-image-file (expand-file-name ".image-dired_temp" saves-dir)))

(use-package ibuf-ext
  :init
  (progn
    (setq ibuffer-expert t)
    (setq ibuffer-show-empty-filter-groups nil)
    (setq ibuffer-saved-filter-groups
          '(("home"
             ("Org" (filename . "\.org$"))
             ("Code" (or (filename . "[Mm]akefile")
                         (mode . python-mode)
                         (mode . c-mode)
                         (mode . lua-mode)
                         (filename . "\.js$")))
             ("Web" (or (mode . html-mode)
                        (mode . css-mode)))
             ("Emacs" (or (filename . ".emacs.d")
                          (filename . "\.el$")))
             ("Magit" (name . "\*magit"))
             ("ERC" (mode . erc-mode))
             ("Help" (or (name . "\*Help\*")
                         (name . "\*Apropos\*")
                         (name . "\*info\*"))))))
    (add-hook 'ibuffer-mode-hook
              '(lambda ()
                 (ibuffer-auto-mode 1)
                 (ibuffer-switch-to-saved-filter-groups "home"))))
  :bind ("C-x C-b" . ibuffer))

(use-package ledger)

(use-package smex
  :disabled noninteractive
  :config
  (progn
    (smex-initialize)
    (define-key global-map (kbd "M-x") 'smex)
    (define-key global-map (kbd "M-X") 'smex-major-mode-commands)
    (define-key global-map (kbd "C-c C-c M-x") 'execute-extended-command)))

(use-package "scim-bridge-dev"
  :disabled noninteractive
  :config
  (progn
    ;; (add-hook 'after-init-hook 'scim-mode-on)
    (scim-define-common-key (kbd "C-`") t)
    ;; Use C-SPC for Set Mark command
    (scim-define-common-key ?\C-\s nil)
    ;; Use C-/ for Undo command
    (scim-define-common-key ?\C-/ nil)))

(use-package yasnippet
  :disabled noninteractive
  :config
  (progn
    (yas-global-mode 1)
    (defun my-yas-activate-js ()
      (yas-activate-extra-mode 'js-mode))
    (add-hook 'espresso-mode-hook 'my-yas-activate-js)
    (add-hook 'javascript-basic-mode 'my-yas-activate-js)))

(use-package hideshow
  :config
  (progn
    (add-hook 'c-mode-common-hook   'hs-minor-mode)
    (add-hook 'lua-mode-hook        'hs-minor-mode)
    (add-hook 'python-mode-hook     'hs-minor-mode)
    (add-hook 'js2-mode-hook        'hs-minor-mode)
    (add-hook 'espresso-mode-hook   'hs-minor-mode)
    (add-hook 'perl-mode-hook       'hs-minor-mode)
    (add-hook 'sh-mode-hook         'hs-minor-mode)
    (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
    (add-hook 'lisp-mode-hook       'hs-minor-mode)
    (add-hook 'java-mode-hook       'hs-minor-mode)
    (define-key hs-minor-mode-map (kbd "C-<f1>") 'hs-toggle-hiding)))

(use-package color-theme
  :config
  (or
   (use-package lilacs
     :config (color-theme-lilacs))
   (use-package "inkpot"
     :if window-system
     :config (color-theme-inkpot))
   (use-package "twilight"
     :if window-system
     :config (color-theme-twilight))))

(use-package command-frequency
  :disabled noninteractive
  :init
  (setq command-frequency-table-file
        (concat saves-dir ".emacs.frequencies"))
  :config
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

(use-package ess-site
  :commands (R ess-eval-buffer ess-make-buffer-current))

(use-package go-mode-load
  :load-path "/usr/local/go/misc/emacs"
  :mode ("\\.go\\'" . go-mode)
  :interpreter ("go" . go-mode))

(use-package abbrev
  :disabled noninteractive
  :config
  (progn
  ;; (setq abbrev-file-name (concat user-emacs-directory ".abbrev_defs"))
  (setq abbrev-file-name (expand-file-name ".abbrev_defs" saves-dir))
  (if (file-exists-p abbrev-file-name) (read-abbrev-file))
  (setq save-abbrevs t)))

(use-package bookmark
  :config (setq bookmark-default-file
                (concat user-emacs-directory ".emacs.bmk"))) 

(use-package jabber
  :load-path "vendor/jabber"
  :disabled noninteractive
  :commands (jabber-connect jabber-connect-all))

;; System, host and user specific configuration
(setq my-configs
      (list "init"
            (if (eq system-type 'gnu/linux) "linux" (symbol-name system-type))
            system-name user-login-name
            "ext-custom" "org-custom"))
(setq my-config-files
      (mapcar (lambda (x) (expand-file-name x user-emacs-directory))
              my-configs))
(dolist (config (cdr my-config-files)) ; skip init.el
  (load config 'noerror))

(defvar user-specific-config
  (expand-file-name user-login-name user-emacs-directory))
(when (file-exists-p user-specific-config)
  (dolist (file (directory-files user-specific-config 'noerror ".*el$"))
    (or (load (concat file "c") 'noerror) (load file))))

(use-package desktop
  :disabled noninteractive
  :init
  (progn
    (setq history-length 250)
    (setq desktop-restore-eager 1)      ; actually affects how it is saved
    (add-to-list 'desktop-globals-to-save 'file-name-history)
    (add-to-list 'desktop-modes-not-to-save 'Info-mode)
    (add-to-list 'desktop-modes-not-to-save 'dired-mode)
    (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode))
  :config
  (progn
    (add-hook 'desktop-not-loaded-hook (lambda () (desktop-save-mode 0)))
    (desktop-save-mode 1)))             ; this sets after-init hook

;; if everything went ok, compile all init files to start faster next time
(defun my-compile-init-files ()
  (interactive)
  (dolist (file my-config-files)
    (let ((elfile (concat file ".el"))
          (elcfile (concat file ".elc")))
      (when (file-exists-p elfile)
        (unless (and (file-exists-p elcfile)
                     (file-newer-than-file-p elcfile elfile)) 
          (byte-compile-file elfile))))))

(use-package-init-on-idle 'my-compile-init-files 9)
;;; init.el ends here
