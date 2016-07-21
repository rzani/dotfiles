;;----------------------------------------------------------------------------
;; after-load defmacro
;;----------------------------------------------------------------------------
(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


(defun sanityinc/string-rtrim (str)
  "Remove trailing whitespace from `STR'."
  (replace-regexp-in-string "[ \t\n]+$" "" str))

;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;;----------------------------------------------------------------------------
;; Insert lines
;;----------------------------------------------------------------------------
(defun insert-line-before (times)
  "Insert a newline(s) above the line containing the cursor.
You might insert multiple lines using TIMES."
  (interactive "p")
  (save-excursion
    (move-beginning-of-line 1)
    (newline times)))

(defun insert-line-after (times)
  "Insert a newline(s) below the line containing the cursor.
You might insert multiple lines using TIMES."
  (interactive "p")
  (save-excursion
    (move-end-of-line 1)
    (newline times)))

;;----------------------------------------------------------------------------
;; Charactes end-of-line - semicolon
;;----------------------------------------------------------------------------
(defun rzani/add-semicolon-end-of-line()
  "Add semicolon at the end of the line and return to current position"
  (interactive)
  (save-excursion
    (end-of-line)
    (if (not (= (preceding-char) 59)) (progn (insert ";") (evil-normal-state)))))

(global-set-key (kbd "C-M-;") 'rzani/add-semicolon-end-of-line)

;;----------------------------------------------------------------------------
;; Charactes end-of-line - colon
;;----------------------------------------------------------------------------
(defun rzani/add-comma-end-of-line()
  "Add comma at the end of the line and return to current position"
  (interactive)
  (save-excursion
    (end-of-line)
    (if (not (= (preceding-char) 44)) (progn (insert ",") (evil-normal-state)))))

(global-set-key (kbd "C-M-,") 'rzani/add-comma-end-of-line)

;;----------------------------------------------------------------------------
;; Switch to previous buffer
;;----------------------------------------------------------------------------
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(provide 'init-utils)
