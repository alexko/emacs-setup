;; work around emacs bug#5645 (read-event blocks in batch mode)
(defadvice sit-for
  (around my-sit-for activate)
  "Be non-interactive while starting a daemon."
  (if (and (daemonp) (not (boundp 'server-process)))
           (let ((noninteractive t)) ad-do-it) ad-do-it))

(defun f-toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (if selective-display nil
     (or column (+ 1 (current-column))))))

(define-key global-map (kbd "<f1>") 'f-toggle-selective-display)

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

(define-key global-map (kbd "C-=") 'my-calc-eval)

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

;; see also M-x delete-trailing-whitespace
(defun remove-ws-in-region (n)
  "Removes trailing or leading whitespace in the region"
  (interactive "p")
  (save-match-data
    (save-excursion
      (save-restriction
        (narrow-to-region (point) (mark))
        (goto-char (point-min))
        (while (re-search-forward
                (if (= n 4) "^[ 	]+" "[ 	]+$") nil t)
          (replace-match "" nil nil))))))

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

(when (equal system-type 'darwin)
  ;; from tim_custom.el
  ;; When started from Emacs.app or similar, ensure $PATH
  ;; is the same the user would see in Terminal.app
  (defun path-from-shell-PATH ()
    (let ((path-from-shell
           (shell-command-to-string "$SHELL -c 'echo $PATH'")))
      (split-string path-from-shell path-separator)))
  (when window-system
      (setenv "PATH" path-from-shell)
      (setq exec-path (path-from-shell-PATH))))

(provide 'alex-custom)
