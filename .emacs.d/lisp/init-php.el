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

  (add-to-list 'write-file-functions 'delete-trailing-whitespace)

  )

(use-package php-mode
  :mode "\\.php\\'"
  :config
  (add-hook 'php-mode-hook 'configure-php-mode))

(provide 'init-php)

;;; init-php ends here
