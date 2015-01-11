;; Emacs ELPA Package Manager
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Add the path for loading emacs modules.
(add-to-list 'load-path "~/.emacs.d/")
(require 'cc-mode)
(require 'google-c-style)
(require 'column-marker)
(require 'cmake-mode)
(require 'protobuf-mode)
(require 'php-mode)

;; Highlight 80 columns
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)
;; Show column numbers
(setq column-number-mode t)

;; Google C++ Style
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
;; Use c++-mode syntax highlighting for .h
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; JS/JSON set tab to 2
(setq js-indent-level 2)

;; CMake
;; Add cmake listfile names to the mode list.
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))
;; Autoload protofiles to the mode list
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;; PHP syntax highlighting for .inc files
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; Load the TAGS file.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Select-Tags-Table.html#Select-Tags-Table
(setq tags-table-list '("/work/projects/beeswaxio/honeycomb"))

;; Bind F7 to find-tag
(global-set-key [f7] 'find-tag)
;; Bind F8 to pop back
(global-set-key [f8] "\M-*")
;; Bind F9 to continue to next occurrence.
(global-set-key [f9] "\C-u\M-.")
