;;; init-evil.el --- Configure the evil-mode and dependences

;;; Commentary:
;;;

;;; Code:
(require-package 'avy)
(require-package 'evil)
(require-package 'evil-leader)
(require-package 'evil-indent-textobject)
(require-package 'evil-surround)
(require-package 'evil-mc)

(require 'evil)
(require 'avy)

(require 'evil-indent-textobject)
(require 'evil-surround)

(global-evil-surround-mode)

;;------------------------------------------------------------------------------
;; Define S-SPC as <escape>
;;------------------------------------------------------------------------------
(defun evil-esc (prompt)
  (cond
   ((or (evil-insert-state-p) (evil-normal-state-p) (evil-replace-state-p) (evil-visual-state-p)) [escape])
   (t (kbd "C-g"))))

(define-key key-translation-map (kbd "S-SPC") 'evil-esc)
(define-key evil-operator-state-map (kbd "S-SPC") 'keyboard-quit)
(set-quit-char "S-SPC")
;;------------------------------------------------------------------------------
;; Load evil-mode
;;------------------------------------------------------------------------------
(evil-mode 1)

;;------------------------------------------------------------------------------
;; Load and config evil-leader
;;------------------------------------------------------------------------------
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(setq evil-leader/in-all-states 1)
(evil-leader/set-key
  ","   'avy-goto-char-2
  "."   'switch-to-previous-buffer
  "h"   'split-window-below
  "o"   'delete-other-windows
  "q"   'kill-this-buffer
  "r"	'copy-to-register
  "v"   'split-window-right
  "w"   'save-buffer
  "y"	'insert-register
)

(provide 'init-evil)
;;; init-evil ends here
