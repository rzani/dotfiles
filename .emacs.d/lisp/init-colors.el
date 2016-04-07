;;; init-colors --- Add background color to hex values

;;; Commentary:

;;; Code:

(defun syntax-color-hex ()
  "Syntax color text of the form 「#ff1100」 in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2016-03-15"
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[ABCDEFabcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer))

(add-hook 'css-mode-hook 'syntax-color-hex)
(add-hook 'php-mode-hook 'syntax-color-hex)
(add-hook 'html-mode-hook 'syntax-color-hex)

(provide 'init-colors)

;;; init-colors.el ends here
