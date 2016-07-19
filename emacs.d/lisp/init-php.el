(defun configure-php-mode ()
  "Set up all of my PHP mode preferences."

  (setq flycheck-disabled-checkers '(php-phpmd))

  (flycheck-mode)
  (yas-minor-mode t)
  (emmet-mode 1)
  (smartparens-mode t)
  (linum-mode)
  (turn-on-auto-fill)
  (electric-indent-mode)
  (electric-pair-mode)
  (electric-layout-mode)

  (setq indent-tabs-mode nil
        c-basic-offset 4
        php-mode-coding-style 'psr2
        tab-width 4
        fill-column 80))

(require-package 'php-mode)
(add-hook 'php-mode-hook 'configure-php-mode)

;; @todo find better place for it - Removes trailing whitespace
(add-hook 'prog-mode-hook (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(defun rzani/php-single-arrow ()
  "Inserts -> at point"
  (interactive)
  (insert "->"))

(defun rzani/php-double-arrow ()
  "Inserts => at point"
  (interactive)
  (insert "=>"))

(require-package 'auto-complete)
(require-package 'php-auto-yasnippets)

(ac-config-default)
(require 'php-auto-yasnippets)

(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)
(define-key php-mode-map (kbd "C--") 'rzani/php-single-arrow)
(define-key php-mode-map (kbd "C-=") 'rzani/php-double-arrow)

(provide 'init-php)

;;; init-php ends here
