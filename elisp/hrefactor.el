;;; hrefactor.el --- ripped from hindent and haskell-ext
;;; Code:

(require 'cl-lib)

(defvar hrefactor-refactor-prefix
  "refactor-"
  "prefix for refactor file")

(defvar hrefactor-style
  "tony-day"
  "this should be refactored out")

;;;###autoload
(defun hrefactor-all ()
  "reformat, then fix flychecks."
  (interactive)
  (save-excursion
    (hrefactor-reformat-module)
    (flycheck-buffer)
    (hrefactor-flycheck-fix-all)
    (haskell-navigate-imports)
    (haskell-align-imports)))

;;;###autoload
(defun hrefactor-reformat-module ()
  "Re-format the entire module at once."
  (interactive)
  (let ((start-end (cons (point-min) (point-max))))
    (when start-end
      (let ((original (current-buffer))
            (orig-str (buffer-substring-no-properties (car start-end)
                                                      (cdr start-end))))
        (with-temp-buffer
          (let ((temp (current-buffer)))
            (with-current-buffer original
              (let ((ret (call-process-region (car start-end)
                                              (cdr start-end)
                                              "hrefactor"
                                              nil  ; delete
                                              temp ; output
                                              nil
                                              "--style"
                                              hrefactor-style)))
                (cond
                 ((= ret 1)
                  (let ((error-string
			 (with-current-buffer temp
                           (let ((string (progn (goto-char (point-min))
                                                (buffer-substring (line-beginning-position)
                                                                  (line-end-position)))))
                             string))))
		    (if (string= error-string "hrefactor: Parse error: EOF")
			(message "language pragma")
		      (error error-string))))
                 ((= ret 0)
                  (let ((new-str (with-current-buffer temp
                                   (buffer-string))))
                    (if (not (string= new-str orig-str))
                        (progn
                          (delete-region (car start-end)
                                         (cdr start-end))
                          (insert new-str)
                          (message "Formatted."))
                      (message "Already formatted.")))))))))))))

;; flycheck functionality
;;;###autoload
(defun hrefactor-flycheck-fix-all ()
  "Fix all flycheck errors in buffer."
  (interactive)
  (save-excursion
    (let ((buf
           (create-file-buffer (format "%s%s"
                                       hrefactor-refactor-prefix
                                       (buffer-name)))))
    
      (goto-char (point-min))
      (while (< (point) (point-max))
        (hrefactor-fix-then-next-err buf))
      (with-current-buffer buf (copy-region-as-kill (point-min) (point-max)))
      (goto-char (point-min))
      (yank)
      (kill-region (point) (point-max)))))

(defun hrefactor-fix-then-next-err (buf)
  "fix errors at point (if possible) then copy to next flycheck error"
  (let* ((start (point))
         (next (flycheck-next-error-pos 1)
               )
         (end (if next
                  next
                (point-max))))
    (let ((errs (flycheck-overlay-errors-at start)))
      (while (and
              errs
              (= start (point)))
      (let ((err (car errs)))
        (setq errs (cdr errs))
        (hrefactor-fix-err err buf))))
    
    ;; point might already be past next error
    (when (< (point) end)
      (copy-region-as-kill (point) end)
      (goto-char end)
      (with-current-buffer buf (yank)))))

(defun hrefactor-fix-err (err buf)
  "fix err, moving point past the fixed code, if any"
  (let* ((msg (flycheck-error-message err))
         (spot (flycheck-error-line err)))
    (cond ((string-match (haskell-ext-flycheck-typemessage-re) msg)
           (with-current-buffer buf
             (insert
              (hrefactor-flycheck-clean-sig
               (match-string 1 msg)))))
          ((string-match (hrefactor-flycheck-redundant-import-re)
                         msg)
           (hrefactor-move-to-next-import))
          ((string-match (hrefactor-flycheck-whynot-re)
                         msg)
           (hrefactor-flycheck-replace-whynot err buf)))))

(defun hrefactor-move-to-next-import ()
  "move point to next import statement, or next line if no more imports"
  (progn
    (let*
        ((start (point)))
      (if (search-forward-regexp "\nimport" (point-max) t)
          (goto-char (+ 1 (match-beginning 0)))
        (when (search-forward-regexp "\n" (point-max) t))
        (goto-char (+ 1 (match-beginning 0)))))))

(defun hrefactor-flycheck-replace-whynot (err buf)
  (let* ((msg (flycheck-error-message err))
         (spot (flycheck-error-line err))
         (parts
          (mapcar (lambda (x) (mapcar 'hrefactor-flycheck-fix-lambda x)) (hrefactor-flycheck-hlint-parts msg)))
         (re (hrefactor-flycheck-main-re parts)))
    (when (search-forward-regexp re (point-max) t)
      (with-current-buffer buf
        (insert
         (reduce
          #'concat
          (hrefactor-flycheck-interleave
           (nth 1 parts)
           (mapcar
            'match-string
            (number-sequence 1 (- (length (nth 1 parts)) 1))))))))))

;; in place methods
(defun hrefactor-flycheck-fix-hlints-at-point ()
  "Insert flycheck hint at point."
  (interactive)
  (let ((errs (flycheck-overlay-errors-at (point))))
    (when errs
      (-each errs 'hrefactor-flycheck-fix-hlint))))

(defun hrefactor-flycheck-fix-hlint (err)
  (save-excursion
    (let* ((msg (flycheck-error-message err))
           (spot (flycheck-error-line err)))
      (cond ((string-match (hrefactor-flycheck-typemessage-re) msg)
             (insert
              (hrefactor-flycheck-clean-sig
               (match-string 1 msg))))
            ((string-match (hrefactor-flycheck-redundant-import-re)
                           msg)
             (delete-to-next-import))
            )
      )))

(defun delete-to-next-import ()
  "delete region from point to next import statement"
  (progn
         (let*
             ((start (point)))
           (if (search-forward-regexp "\nimport" (point-max) t)
               (delete-region start (+ 1 (match-beginning 0)))
             (when (search-forward-regexp "\n" (point-max) t))
             (delete-region start (+ 1 (match-beginning 0)))) 
           )
         ))


(defun hrefactor-flycheck-insert-why-nots ()
  "Insert flycheck found/why not? hint at point."
  (interactive)
  (let ((errs (flycheck-overlay-errors-at (point))))
    (when errs
      (-each errs 'hrefactor-flycheck-insert-lint))))

(defun hrefactor-flycheck-insert-type-bindings ()
  "Insert all flycheck type binding guesses."
  (interactive)
  (goto-char (point-min))
  (message
        "added %d type sigs"
        (fromMaybe 0 (let*
             ((added 0)
              (changes
               (mapcar
                (lambda (x)
                  (when
                      (hrefactor-flycheck-is-type-bind x)
                    (goto-line
                     (+ added
                        (flycheck-error-line x)))
                    (hrefactor-flycheck-insert-sig-at-point x)
                    (setq added (+ 1 added))))
                flycheck-current-errors)))
           (car (last (delq nil changes)))))))

(defun hrefactor-flycheck-insert-type-binding ()
  "Insert the flycheck guess at the type binding."
  (interactive)
  (hrefactor-flycheck-insert-sig (flycheck-overlay-errors-at (point))))

(defun hrefactor-flycheck-insert-sig-at-point (err)
  (let ((msg
         (hrefactor-flycheck-get-sig (flycheck-error-message err))))
    (when msg
      (insert (hrefactor-flycheck-clean-sig msg)))))

(defun hrefactor-flycheck-insert-sig (errs)
  (when errs
    (-each errs
           (lambda (e)
             (let ((msg
                    (hrefactor-flycheck-get-sig (flycheck-error-message e))))
               (when msg
                 (insert (hrefactor-flycheck-clean-sig msg))))))))

(defun hrefactor-flycheck-is-type-bind (e)
  (string-match
   (hrefactor-flycheck-typemessage-re)
   (flycheck-error-message e)))

(defun hrefactor-flycheck-get-sig (msg)
  (when (string-match
         (hrefactor-flycheck-typemessage-re)
         msg)
    (match-string 1 msg)))

(defun hrefactor-flycheck-clean-sig (msg)
  (with-temp-buffer
  (insert msg)
  (goto-char (point-min))
  (replace-regexp
   (hrefactor-flycheck-forall-re)
   "")
  (goto-char (point-min))
  (replace-regexp (rx "=>") "=> ")
  (goto-char (point-min))
  (replace-regexp
   (rx
    (and
     (1+ "\n")
     (0+ blank)))
   "")
  (goto-char (point-max))
  (insert "\n")
  (buffer-string)))

(defun hrefactor-flycheck-forall-re ()
  (rx
   (and
    "forall"
    (1+ any)
    (1+ ".")
    (0+ blank))))

(defun hrefactor-flycheck-typemessage-re ()
  (rx
   (and
    "Top-level binding with no type signature:"
    (0+ "\n")
    (0+ blank)
    (group
     (0+ (or
          any
          "\n"))))))

(defun hrefactor-flycheck-redundant-import-re ()
  (rx
   (and
    (or
     "The import of "
     "The qualified import of ")
    (1+ not-newline)
    " is redundant")))

(defun hrefactor-flycheck-whynot-re ()
  (rx
   (and
    "Found:\n"
    (0+ blank)
    (group (1+ (or any "\n")))
    (1+ "\n")
    "Why not:\n"
    (0+ blank)
    (group (1+ (or any "\n"))))))

(defun hrefactor-flycheck-hlint-re ()
  (rx
   (and
    "Found:\n"
    (0+ blank)
    (group (1+ (or any "\n")))
    (1+ "\n")
    "Why not:\n"
    (0+ blank)
    (group (1+ (or any "\n"))))))

(defun hrefactor-flycheck-hlint-line-re ()
  (rx
   (and
    (0+ blank)
    (group (1+ any)))))

(defun hrefactor-flycheck-lint-lines (str)
  (with-temp-buffer
    (goto-char (point-min))
    (kill-region (point) (point-max))
    (insert str)
    (goto-char (point-min))
    (let ((parts nil))
      (while
          (search-forward-regexp (hrefactor-flycheck-hlint-line-re) (point-max) t)
        (push (match-string 1) parts))
      (reverse parts))))

(defun hrefactor-flycheck-hlint-parts (hlint-msg)
  (with-temp-buffer
    (insert hlint-msg)
    (goto-char (point-min))
    (search-forward-regexp (hrefactor-flycheck-hlint-re) (point-max) t)
    ;; get individual lines
    (let* ((sfound (match-string 1))
           (swhynot (match-string 2)))
      (list
       (hrefactor-flycheck-lint-lines sfound)
       (hrefactor-flycheck-lint-lines swhynot)))))

(defun hrefactor-flycheck-main-re (parts)
  (mapconcat
   'identity
   (mapcar
    (lambda (x) (regexp-opt (list x)))
    (car parts))
   "\\(\\(?:[[:blank:]]\\|\n\\)+\\)"))

(defun hrefactor-flycheck-fix-lambda (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (if (search-forward-regexp "\\\\ " (point-max) t)
        (progn
          (replace-match "\\\\")
            (buffer-string))
      str)))

(defun hrefactor-flycheck-insert-lint (err)
  (save-excursion
    (let* ((msg (flycheck-error-message err))
           (spot (flycheck-error-line err))
           (parts
            (mapcar (lambda (x) (mapcar 'hrefactor-flycheck-fix-lambda x)) (hrefactor-flycheck-hlint-parts msg)))
           (re (hrefactor-flycheck-main-re parts)))
      (when (search-forward-regexp re (point-max) t)
        (replace-match
         (reduce
          #'concat
          (hrefactor-flycheck-interleave
           (nth 1 parts)
           (mapcar
            'match-string
            (number-sequence 1 (- (length (nth 1 parts)) 1))))))))))

(defun hrefactor-flycheck-interleave (l1 l2)
"(hrefactor-flycheck-interleave (list 1 1 1) (list 0 0))  '(1 0 1 0 1)."
  (cond ((and (eql l1 nil) (eql l2 nil)) nil)         ;; rule #1
        ((eql l1 nil) (cons nil (hrefactor-flycheck-interleave l2 l1)))  ;; rule #2, current value is nil
        (t (cons (first l1) (hrefactor-flycheck-interleave l2 (cdr l1)))))) ;; rule #3 in all
;; other cases

(defun fromMaybe (default x)
  (if x
      x
    default))


(provide 'hrefactor)

;;; hrefactor.el ends here
