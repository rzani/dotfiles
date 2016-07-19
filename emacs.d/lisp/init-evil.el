;; EVIL - Emacs VIm Layer
(require-package 'evil)

;; Apply vim leader feature
(require-package 'evil-leader)

;; Indent text object like vim
(require-package 'evil-indent-textobject)

;; Add surround.vim for evil
(require-package 'evil-surround)

;; Multiples Cursors for evil
(require-package 'evil-mc)

;; Add nerdcommenter.vim for evl
(require-package 'evil-nerd-commenter)

;;------------------------------------------------------------------------------
;; Config EVIL Leader
;;------------------------------------------------------------------------------
(defun rzani/config-evil-leader()
  "Configure evil leader mode."
  (evil-leader/set-leader ",")
  (setq evil-leader/in-all-states 1)
  (require-package 'avy)
  (evil-leader/set-key
    "."   'switch-to-previous-buffer
    ","   'avy-goto-char-2
    "SPC" 'avy-goto-word-1
    "_"   'split-window-vertically-instead
    "|"   'split-window-horizontally-instead
    ">"   'sgml-skip-tag-forward
    "<"   'sgml-skip-tag-backward
    "1"   'delete-other-windows
    "b"   'helm-mini
    "d"   'whitespace-cleanup
    "e"   'eval-buffer
    "f"   'find-file
    "g"   'magit-status
    "h"   'split-window-below
    "j"   'bookmark-jump
    "l"   'whitespace-mode
    "p"   'insert-register
    "q"   'kill-this-buffer
    "r"   'helm-recentf
    "s"   'ag-project
    "t"   'gtags-reindex
    "T"   'helm-gtags-find-tag
    "v"   'split-window-right
    "w"   'save-buffer
    "y"   'copy-to-register
    "x"   'helm-M-x
    "ci"  'evilnc-comment-or-uncomment-lines
    "cl"  'evilnc-quick-comment-or-uncomment-to-the-line
    "cc"  'evilnc-copy-and-comment-lines
    "cp"  'evilnc-comment-or-uncomment-paragraphs
    "cr"  'comment-or-uncomment-region
    "cv"  'evilnc-toggle-invert-comment-line-by-line
    "\\"  'evilnc-comment-operator ; to comment n lines
    )
  )

;;------------------------------------------------------------------------------
;; Config EVIL
;;------------------------------------------------------------------------------
(defun rzani/config-evil()
  "Configure EVIL mode"

  (delete 'term-mode evil-insert-state-modes)

  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

  ;; Global bindings.
  (define-key evil-normal-state-map (kbd "<down>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<up>")   'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "-")      'helm-find-files)
  (define-key evil-normal-state-map (kbd "C-]")    'gtags-find-tag-from-here)
  (define-key evil-normal-state-map (kbd "g/")     'occur-last-search)
  (define-key evil-normal-state-map (kbd "[i")     'show-first-occurrence)
  (define-key evil-motion-state-map (kbd "C-o")    'insert-line-after)

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
)


;;------------------------------------------------------------------------------
;; Loads EVIL
;;------------------------------------------------------------------------------
(add-hook 'evil-mode-hook 'rzani/config-evil)
(evil-mode 1)
(global-evil-leader-mode)
(rzani/config-evil-leader)
(global-evil-surround-mode)
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

(provide 'init-evil)
