;;; init.el --- Where all the magic begins

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(if (equal system-type 'darwin)
    (setq ns-option-modifier  'super
          ns-command-modifier 'meta))

;; Load path, should it just use user-emacs-directory?
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(let ((default-directory (concat dotfiles-dir "vendor/")))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit/jabber"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor"))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))
(setq cruft-dir "~/.emacs.cruft/")

;;(regen-autoloads)
(load autoload-file)
(load custom-file 'noerror)

(require 'use-package)
(require 'cl)
(use-package ansi-color)

(use-package package
  :init (package-initialize))

(use-package ffap
;; :init (ffap-bindings) ; these conflict with ido bindings
 :init (setq ido-use-filename-at-point 'guess))

(use-package uniquify
  "more descriptive buffer names if the file name is the same"
  :init (setq uniquify-buffer-name-style 'forward))

(use-package recentf
  :config (recentf-mode 1)
  :commands recentf-list
  :bind ("C-x f" . recentf-open-files))

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

(use-package tramp
  :init
  (setq tramp-auto-save-directory (concat cruft-dir "auto-saves/")
        tramp-backup-directory-alist backup-directory-alist
        shell-prompt-pattern "[^\n]*\\([>#$%][ ]+\\)+$")
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "root" "/ssh:%h:")))

(use-package desktop
  :init
  (add-hook 'desktop-not-loaded-hook (lambda () (desktop-save-mode 0)))
  :config ; this sets after-init hook
  (desktop-save-mode 1))

(use-package saveplace
  "saves the cursor position for every opem file"
  :disabled t ; desktop saves these as well
  :init
  (setq save-place-file (concat user-emacs-directory "saveplace.el"))
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

;; System and user specific configs
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))

(use-package tim-custom)
(use-package alex-custom)
(use-package setup-erc)
;;; init.el ends here
