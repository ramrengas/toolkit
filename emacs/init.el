(require 'cc-mode)

;; Load the TAGS file.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Select-Tags-Table.html#Select-Tags-Table
(setq tags-table-list '("~/rtbkit"))

;; Bind F7 to find-tag
(global-set-key [f7] 'find-tag)
;; Bind F8 to pop back
(global-set-key [f8] "\M-*")
