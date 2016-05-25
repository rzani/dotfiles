;;; init.el --- Provide a init file to emacs

;;; Commentary:
;;;

;;; Code:

; Load .emacs.d/lisp directory
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(require 'init-version)

(require 'init-package)

(use-package mmm-mode
  :ensure t
  :defer t)

(require 'init-interface)

(require 'init-utils)

(require 'init-flycheck)

(require 'init-helm)

(require 'init-multiple-cursors)

(require 'init-move-text)

;; ;(require 'init-whitespace)
;; (require 'init-editorconfig)

(require 'init-linum)

;; (require 'init-powerline)
(require 'init-spaceline)

(require 'init-neotree)

(require 'init-smartparens)

(require 'init-fiplr)

(require 'init-php)

(require 'init-colors)

(require 'init-evil)

(require 'init-markdown)

(require 'init-web)

(require 'init-windows)

(use-package swiper
  :ensure t
  :commands swiper
  :bind ("C-s" . swiper))

(use-package yaml-mode
  :ensure t
  :config
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  ;; new line and indent
  (add-hook 'yaml-mode-hook
      '(lambda ()
	(define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/bin/zsh")
  (add-hook 'term-mode-hook
	    (lambda ()
	      (setq term-buffer-maximum-size 10000))))




(global-set-key (kbd "C-M-;") 'rzani/add-semicolon-end-of-line)
(global-set-key (kbd "C-M-,") 'rzani/add-comma-end-of-line)

;; -----------------------------------------------------------------------------
;; Macos stuff
;; -----------------------------------------------------------------------------
(when *is-a-mac*
  (set-default-font "Fira Mono for Powerline 14")	; Change default font
  (global-unset-key (kbd "<home>"))                     ; Unset home key binding
  (global-set-key (kbd "<home>") 'beginning-of-line)    ; Set home key to beginning of line
  (global-unset-key (kbd "<end>"))                      ; Unset end key binding
  (global-set-key (kbd "<end>") 'end-of-line)           ; Set end key to end of line
  (setq ns-use-srgb-colorspace nil))

(provide 'init)

;;; init ends here
