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
(require-package 'evil-nerd-commenter)

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
(evil-leader/set-key
  "."   'switch-to-previous-buffer
  "h"   'split-window-below
  "o"   'delete-other-windows
  "q"   'kill-this-buffer
  "r"	'copy-to-register
  "v"   'split-window-right
  "w"   'save-buffer
  "y"	'insert-register
  "ci"  'evilnc-comment-or-uncomment-lines
  "cl"  'evilnc-quick-comment-or-uncomment-to-the-line
  "ll"  'evilnc-quick-comment-or-uncomment-to-the-line
  "cc"  'evilnc-copy-and-comment-lines
  "cp"  'evilnc-comment-or-uncomment-paragraphs
  "cr"  'comment-or-uncomment-region
  "cv"  'evilnc-toggle-invert-comment-line-by-line
  "\\"  'evilnc-comment-operator ; if you prefer backslash key
  )

  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

;; Make escape quit everything, whenever possible.
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(define-key evil-motion-state-map (kbd "C-o") 'insert-line-after)
(define-key evil-motion-state-map (kbd "C-S-o") 'insert-line-before)
(provide 'init-evil)
;;; init-evil ends here
