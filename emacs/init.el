;; Emacs ELPA Package Manager
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(require 'package)
(package-initialize)
(unless (package-installed-p 'scala-mode2)
  (package-refresh-contents) (package-install 'scala-mode2))

(require 'uniquify)

;; Zenburn theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; Add the path for loading emacs modules.
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'cl)
(require 'cc-mode)
(require 'google-c-style)
(require 'column-marker)
(require 'cmake-mode)
(require 'protobuf-mode)
(require 'php-mode)
(require 'dockerfile-mode)

;; Highlight 80 columns
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face trailing lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)
;; Show column numbers
(setq column-number-mode t)

;; Google C++ Style
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
;; Use c++-mode syntax highlighting for .h
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c-initialization-hook (lambda () (define-key c-mode-base-map [f7] 'ff-get-other-file)))

;; JS/JSON set tab to 2
(setq js-indent-level 2)

;; CMake
;; Add cmake listfile names to the mode list.
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))
;; BUILD
(add-to-list 'auto-mode-alist '("BUILD" . python-mode))
(add-to-list 'auto-mode-alist '("WORKSPACE" . python-mode))
(add-to-list 'auto-mode-alist '("\\.bzl\\'" . python-mode))

;; Autoload protofiles to the mode list
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;; PHP syntax highlighting for .inc files
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; Dockerfile
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
