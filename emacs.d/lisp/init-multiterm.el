(require-package 'multi-term)

(setq multi-term-program "/bin/zsh")

;; Term tweak
(add-hook 'term-mode-hook (lambda() (yas-minor-mode -1)))
(add-hook 'shell-mode (lambda() (yas-minor-mode -1)))

(evil-set-initial-state 'term-mode 'emacs)

(provide 'init-multiterm)
;;; init-multiterm ends here
