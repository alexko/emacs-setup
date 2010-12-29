(require 'comint)

(defgroup tt-lua nil
  "Options for connecting to tt-lua"
  :tag "TT Shell"
  :group 'lua)

(defcustom tt-default-thost "localhost"
  "Default host running ttserver"
  :type 'string
  :group 'tt-lua)

(defcustom tt-default-tport 1978
  "Default port used by ttserver"
  :type 'integer
  :group 'tt-lua)

(defcustom tt-default-rport 1999
  "Default port for repl to listen"
  :type 'integer
  :group 'tt-lua)

(defcustom tt-proxy-cmd ""
  "Command to use when we can't access ttserver directly, e.g. ssh user@gateway"
  :type 'string
  :group 'tt-lua)

(defun tt-lua (spec)
  ;; todo: override the default proxy on C-u?
  "Interactive Lua repl for ttserver"
  (interactive (let ((host-port (read-from-minibuffer (concat "Host:port (" tt-default-thost ":" (int-to-string tt-default-tport) ") "))))
                 (list host-port)))
  (defun mapper (x) (cond ((equal x "") nil) ((string-match "^[0-9]+$" x) (string-to-number x)) (t x)))
  (apply 'tt-repl (mapcar 'mapper (split-string spec ":"))))

(defun tt-repl (&optional thost tport rport lcmd)
  "Lua repl for ttserver"
  (let* (
         (thost (or thost tt-default-thost))
         (stport (int-to-string (or tport tt-default-tport)))
         (srport (int-to-string (or rport tt-default-rport))) ; this won't work if tport is 1999
         (proxy (split-string tt-proxy-cmd))
         (tcr (append proxy (list "tcrmgr" "ext" "-port" stport thost "repl" srport)))
         (cmd (append proxy (list "nc" "-q5" thost srport)))
         (pname (concat thost ":" stport))
         (wname (concat "*" pname "*"))
         (tproc (apply 'start-process " ttserver" "ttserver" tcr)))
    (sleep-for 0 500)
    (if (equal 'run (process-status tproc))
        (progn
          (delete-other-windows)
          (switch-to-buffer-other-window wname)
          (insert (concat "Lua repl at " pname "\n"))
          (apply 'make-comint pname (car cmd) nil (cdr cmd))
          (if lcmd (comint-send-string
                    (get-buffer-process wname) (concat lcmd "\n")))))))

(provide 'tt-lua)
