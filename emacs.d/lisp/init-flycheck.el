(when (maybe-require-package 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode)

  ;; Override default flycheck triggers
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
	flycheck-idle-change-delay 0.8)

  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(eval-after-load 'flycheck
  '(progn
     (diminish 'flycheck-mode "Fly")
     'flycheck-mode))

(eval-after-load 'undo-tree
  '(progn
     (diminish 'undo-tree-mode "UT")
     'undo-tree-mode))

(provide 'init-flycheck)
