;;; init-web.el --- web-mode settings and preferences

;;; Commentary:

;;; Code:

(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig$" . web-mode))

(use-package dictionary :ensure t)

(use-package emmet-mode :ensure t)

(use-package web-mode
  :ensure t
  :defer t
  :config
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-sql-indent-offset 2))

;;; Web mode:
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (setq web-mode-style-padding 2)
;;             (yas-minor-mode t)
;;             (emmet-mode)
;;             (flycheck-add-mode 'html-tidy 'web-mode)
;;             (flycheck-mode)))

(provide 'init-web)

;;; init-web ends here
