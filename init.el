;;; init.el --- Where all the magic begins

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(set-language-environment "UTF-8")
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(add-to-list 'auto-mode-alist (cons "\\.cu$" 'c++-mode))
(show-paren-mode 1)

;; should it just use user-emacs-directory instead of dotfiles-dir?
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq custom-file (concat dotfiles-dir "custom.el"))
(setq cruft-dir "~/.emacs.cruft/")

(setq fill-column 80
      show-trailing-whitespace t
      initial-scratch-message ""
      initial-major-mode 'org-mode
      spell-command "aspell"
      visible-bell t
      tab-width 2
      text-mode-hook '(turn-on-auto-fill text-mode-hook-identify)
      remote-shell-program "/usr/bin/ssh"
      compile-command "cd . ; make -j4 -k"
      frame-title-format "%b %+ %[%f%]"
      icon-title-format "%b"
      auto-save-list-file-prefix (concat cruft-dir "auto-saves/.saves-")
      backup-directory-alist
      (list (cons "." (concat cruft-dir "backups/"))))

;; emacsclient opens new frame, closes when done
(add-hook 'server-switch-hook
          (lambda nil
            (let ((server-buf (current-buffer)))
              (bury-buffer)
              (switch-to-buffer-other-frame server-buf))))
(add-hook 'server-done-hook 'delete-frame)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
;;(define-key global-map (kbd "C-x C-b") 'ibuffer)
(define-key global-map (kbd "M-g") 'goto-line)
(define-key global-map (kbd "M-/") 'hippie-expand)
(define-key global-map (kbd "<f5>") 'edebug-defun)
(define-key global-map (kbd "<C-f9>") 'compile)
(define-key global-map (kbd "<f9>") 'next-error)
(define-key global-map (kbd "C-c o") 'occur)
(define-key global-map (kbd "C-h /") 'find-function)
(define-key occur-mode-map (kbd "q") 'delete-window)

(if (equal system-type 'darwin)
    (setq ns-option-modifier  'super
          ns-command-modifier 'meta))

(let ((default-directory (concat dotfiles-dir "vendor/")))
  (normal-top-level-add-subdirs-to-load-path)) ; this appends

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit/jabber"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor"))

;;(regen-autoloads)
;;(load autoload-file)
(load custom-file 'noerror)

(require 'use-package)
(setq use-package-verbose t)
(require 'cl)
(use-package ansi-color)

(use-package package
  :init (setq package-user-dir (concat dotfiles-dir "elpa")
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

(use-package imenu
  :commands imenu--make-index-alist)

(use-package find-file-in-project
  :init
  (progn
    (setq ffip-patterns '("*.c" "*.h" "*.cc" "*.cpp" "*.cu"
                          "*.py" "*.el" "*.java" "*.js" "*.go"))
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

(use-package espresso
  :init
    (setq espresso-indent-level 2)
  :mode
    (("\\.js$" . espresso-mode) ("\\.json$" . espresso-mode)))

;; begin from tim-custom
(use-package pymacs
  :load-path "vendor/"
  :init (progn
          (autoload 'pymacs-apply "pymacs")
          (autoload 'pymacs-call "pymacs"))
  :commands (pymacs-eval pymacs-exec pymacs-load))

(use-package sgml-mode
  :mode (("\\.html\\'" . html-mode) ("\\.mako\\'" . html-mode)))

(use-package mmm-auto
  :load-path "vendor/mmm-mode"
  :config (setq mmm-global-mode 'maybe))

(use-package mmm-mako
  :load-path "vendor/mmm-mako"
  :config
  (when (functionp 'mmm-add-mode-ext-class)
    (mmm-add-mode-ext-class 'html-mode "\\.mako\\'" 'mako)))

(use-package gccsense)

(use-package google-c-style
  :config
  (progn
    (add-hook 'c-mode-common-hook 'google-set-c-style)
    (add-hook 'c-mode-common-hook 'google-make-newline-indent)))

(use-package py-indent
  :disabled t
  :init (setq-default py-indent-offset 2))
;; end from tim-custom

(use-package idle-highlight
  :init (add-hook 'espresso-mode-hook 'idle-highlight))

(use-package tramp
  :init
  (setq tramp-auto-save-directory (concat cruft-dir "auto-saves/")
        tramp-backup-directory-alist backup-directory-alist
        shell-prompt-pattern "[^\n]*\\([>#$%][ ]+\\)+$")
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "root" "/ssh:%h:")))

(use-package saveplace
  "saves the cursor position for every opem file"
  :init
  (setq save-place-file (concat user-emacs-directory ".emacs-places"))
  (setq-default save-place t))

(use-package ibuf-ext
  :init
  (progn
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
                 (ibuffer-switch-to-saved-filter-groups "home"))))
  :bind ("C-x C-b" . ibuffer))

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
  (use-package lilacs
    :config (color-theme-lilacs)))

(use-package command-frequency
  :init
  (setq command-frequency-table-file
        (concat cruft-dir ".emacs.frequencies"))
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
  :init (setq abbrev-file-name (concat dotfiles-dir ".abbrev_defs"))
  :config (setq save-abbrevs t))

(use-package bookmark
  :config (setq bookmark-default-file (concat dotfiles-dir ".emacs.bmk"))) 

(use-package jabber
  :load-path "vendor/jabber"
  :commands (jabber-connect jabber-connect-all))

(use-package alex-custom)
(use-package org-custom)

(use-package desktop
;;  :disabled t
  :init
  (add-hook 'desktop-not-loaded-hook (lambda () (desktop-save-mode 0)))
  :config
  (progn
    (desktop-save-mode 1) ; this sets after-init hook
    ;; (add-hook 'desktop-after-read-hook 'org-show-context-in-buffers)
    (defun org-show-context-in-buffers ()
      (interactive)
      (save-current-buffer
        (dolist (buffer (buffer-list))
          (when (string-match "\\.org\\'" (buffer-file-name buffer))
            (switch-to-buffer buffer) (org-show-context)))))))

;; System and user specific configs
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))
;;; init.el ends here
