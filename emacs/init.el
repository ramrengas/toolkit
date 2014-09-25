(require 'cc-mode)
(require 'google-c-cstyle)

;; Google C++ Style
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; Use c++-mode syntax highlighting for .h
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))



;; Load the TAGS file.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Select-Tags-Table.html#Select-Tags-Table
(setq tags-table-list '("~/rtbkit"))

;; Bind F7 to find-tag
(global-set-key [f7] 'find-tag)
;; Bind F8 to pop back
(global-set-key [f8] "\M-*")
;; Bind F9 to continue to next occurrence.
(global-set-key [f9] "\C-u\M-.")
