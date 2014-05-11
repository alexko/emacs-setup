;; from tim_custom.el
(defvar ns-option-modifier)
(defvar ns-command-modifier)

(setq ns-option-modifier  'super
      ns-command-modifier 'meta)

;; When started from Emacs.app or similar, ensure $PATH
;; is the same the user would see in Terminal.app
(defun path-from-shell-PATH ()
  (let ((path-from-shell
         (shell-command-to-string "$SHELL -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (path-from-shell-PATH))

(provide 'darwin)