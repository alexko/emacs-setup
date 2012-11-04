(require 'erc)

(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
  '((".*\\.freenode.net" "#cascading" "#cascading-clojure" "#hbase" "#katta" "#clojure")))

(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"

                                 "324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(defun erc-go (password)
  "Connect to ERC, or switch to last active buffer"
  (interactive (list (if (boundp 'irc-password) irc-password
                       (read-passwd "IRC Password: "))))
  (setq irc-password password)
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?
    (erc-track-switch-buffer 1) ;; yes: switch to last active
    (erc :server "irc.freenode.net" :port 6667 :nick "alexko"
         :full-name "alexko" :password password)))

(global-set-key (kbd "C-c e") 'erc-go)

(provide 'setup-erc)
