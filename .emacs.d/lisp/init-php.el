;;; init-php.el --- PHP stuffs

;;; Commentary:

;;; Code:

(defun configure-php-mode ()
  "Set up all of my PHP mode preferences."

  (require 'newcomment)
  (setq comment-auto-fill-only-comments 1)
  (setq auto-fill-function 'do-auto-fill)
  (setq flycheck-disabled-checkers '(php-phpmd))

  (eldoc-mode t)

  (flycheck-mode)
  (yas-minor-mode t)

  (smartparens-mode t)

  (linum-mode)

  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)

  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(use-package php-mode
  :ensure t
  :mode "\\.php\\'"
  :config
  (add-hook 'php-mode-hook 'configure-php-mode))

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default))

(use-package php-auto-yasnippets
  :ensure t
  :config
  (require 'php-auto-yasnippets)
  (define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet))

(provide 'init-php)

;;; init-php ends here
