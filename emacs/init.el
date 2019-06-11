;; Zenburn theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; Unique names for buffers
(require 'uniquify)

;; No tabs, spaces
(setq-default indent-tabs-mode nil)

;; Initialize package
(require 'package)
;; Emacs ELPA Package Manager
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


;; Add the path for loading emacs modules.
(add-to-list 'load-path "~/.emacs.d/lisp/use-package")
(require 'use-package)

;; Ivy
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package swiper
  :after ivy
  :config
  (global-set-key (kbd "C-s") 'swiper))

(use-package counsel
  :after ivy
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char))

(setq ivy-extra-directories nil)
(defun eh-ivy-open-current-typed-path ()
  (interactive)
  (when ivy--directory
    (let* ((dir ivy--directory)
           (text-typed ivy-text)
           (path (concat dir text-typed)))
      (delete-minibuffer-contents)
      (ivy--done path))))

(define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "C-f") 'eh-ivy-open-current-typed-path)

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

;; Markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (ivy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
