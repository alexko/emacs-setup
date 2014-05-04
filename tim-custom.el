(use-package pymacs
  :load-path "vendor/"
  :init (progn
          (autoload 'pymacs-apply "pymacs")
          (autoload 'pymacs-call "pymacs"))
  :commands (pymacs-eval pymacs-exec pymacs-load))

(use-package mmm-mako
  :load-path "vendor/mmm-mode"
  :mode ("\\.mako\\'" . html-mode)
  :config
  (mmm-add-mode-ext-class 'html-mode "\\.mako\\'" 'mako))

(use-package gccsense)

(use-package google-c-style
  :config
  (progn
    (add-hook 'c-mode-common-hook 'google-set-c-style)
    (add-hook 'c-mode-common-hook 'google-make-newline-indent)))

(when (equal system-type 'darwin)
  ;; When started from Emacs.app or similar, ensure $PATH
  ;; is the same the user would see in Terminal.app
  (defun path-from-shell-PATH ()
    (let ((path-from-shell
           (shell-command-to-string "$SHELL -c 'echo $PATH'")))
      (split-string path-from-shell path-separator)))
  (when window-system
      (setenv "PATH" path-from-shell)
      (setq exec-path (path-from-shell-PATH))))

(setq tab-width 2)
(setq rinari-tags-file-name "TAGS")
(setq-default indent-tabs-mode nil)
(setq-default py-indent-offset 2)
(provide 'tim-custom)

