;;; package --- summary
;;; Code:
;;; Commentary:

(require-package 'move-text)

(require 'move-text)
(move-text-default-bindings)

(global-unset-key (kbd "C-M-j"))
(global-unset-key (kbd "C-M-k"))
(global-set-key (kbd "C-M-j") 'move-text-down)
(global-set-key (kbd "C-M-k") 'move-text-up)

(provide 'init-move-text)

;;; init-move-text ends here
