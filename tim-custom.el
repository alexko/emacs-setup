(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))

(add-to-list 'load-path "~/.emacs.d/vendor/mmm-mode")
(require 'mmm-mako)

(add-to-list 'auto-mode-alist '("\\.mako\\'" . html-mode))
(mmm-add-mode-ext-class 'html-mode "\\.mako\\'" 'mako)

(require 'gccsense)

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(defun set-exec-path-from-shell-PATH () 
  (let ((path-from-shell
         (shell-command-to-string "$SHELL -i -c 'echo $PATH'"))) 
    (setenv "PATH" path-from-shell) 
    (setq exec-path (split-string path-from-shell path-separator)))) 

(when (equal system-type 'darwin) 
  ;; When started from Emacs.app or similar, ensure $PATH 
  ;; is the same the user would see in Terminal.app 
  (setq rinari-tags-file-name "TAGS")
  (if window-system (set-exec-path-from-shell-PATH))) 

(setq tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default py-indent-offset 2)
(provide 'tim-custom)

