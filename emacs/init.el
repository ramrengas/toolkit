;; Add the path for loading emacs modules.
(add-to-list 'load-path "~/.emacs.d/")
(require 'cc-mode)
(require 'google-c-style)
(require 'column-marker)
(require 'protobuf-mode)
(require 'tpl-mode)

;; Google C++ Style
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; Use c++-mode syntax highlighting for .h
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; Autoload protofiles to the mode list
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
;; CTemplate Files
(setq auto-mode-alist (cons '("\\.tpl$" . tpl-mode) auto-mode-alist))
(autoload 'tpl-mode "tpl-mode" "Major mode for editing CTemplate files." t)
(add-hook 'tpl-mode-hook '(lambda () (font-lock-mode 1)))

;; Show column numbers
(setq column-number-mode t)
;; Highlight 80 columns
(add-hook 'c-mode-common-hook (lambda () (interactive) (column-marker-1 80)))

;; Load the TAGS file.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Select-Tags-Table.html#Select-Tags-Table
(setq tags-table-list '("~/projects/beeswaxio/honeycomb"))

;; Bind F7 to find-tag
(global-set-key [f7] 'find-tag)
;; Bind F8 to pop back
(global-set-key [f8] "\M-*")
;; Bind F9 to continue to next occurrence.
(global-set-key [f9] "\C-u\M-.")
