;;; init-php.el --- PHP stuffs

;;; Commentary:

;;; Code:

(require-package 'php-mode)
(require-package 'php-auto-yasnippets)
(require-package 'auto-complete)
(eval-after-load 'php-mode
  '(require 'php-ext))

;; AUTO COMPLETE
(ac-config-default)
(require 'php-auto-yasnippets)

(add-hook 'php-mode-hook (lambda ()
      (defun unindent-closure ()
       "Fix php-mode indent for closures"
        (let ((syntax (mapcar 'car c-syntactic-context)))
           (if (and (member 'arglist-cont-nonempty syntax)
             (or
              (member 'statement-block-intro syntax)
              (member 'brace-list-intro syntax)
              (member 'brace-list-close syntax)
              (member 'block-close syntax)))
       (save-excursion
          (beginning-of-line)
          (delete-char (* (count 'arglist-cont-nonempty syntax)
                          c-basic-offset))))))

(defun my-php-lineup-arglist-intro (langelem)
  "Align PHP argument list intro based on LANGELEM."
  (save-excursion
    (goto-char (cdr langelem))
    (vector (+ (current-column) (* 2 c-basic-offset)))))

(defun my-php-lineup-arglist-close (langelem)
  "Align PHP argument list close based on LANGELEM."
  (save-excursion
    (goto-char (c-langelem-pos langelem))
    (vector (current-column))))

(defun my-php-lineup-arglist-cont-nonempty (langelem)
  "Align continued arglist lines to two times the basic offset from LANGELEM."
  (save-excursion
    (goto-char (c-langelem-pos langelem))
    (vector (+ (current-column) (* 2 c-basic-offset)))))

(defun configure-php-mode()
  "Set up all php-mode preferences"
  
  (require 'newcomment)
  (setq comment-auto-fill-only-comments 1)
  (setq auto-fill-function 'do-auto-fill)

  (smartparens-mode)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))


(add-hook 'php-mode-hook 'configure-php-mode)
(add-hook 'c-special-indent-hook 'unindent-closure)
(c-set-offset 'arglist-intro 'my-php-lineup-arglist-intro)
(c-set-offset 'arglist-cont-nonempty 'my-php-lineup-arglist-cont-nonempty)
(c-set-offset 'arglist-close 'my-php-lineup-arglist-close)))

(provide 'init-php)

;;; init-php ends here
