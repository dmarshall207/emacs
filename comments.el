(defun d-set-comment-str (s)
  (interactive
        (list (read-string "comment str: " nil nil "default-val")) )
    (setq comment-str s)) 
(defun d-ins-comm()
  (interactive)
    (let*((col     (cur-col)))
    (insert      comment-str)
    (fwd-line 1)
    (goto (+ (point) col))))
(defun d-rm-comm ()
  (interactive)
  (let*( (col  (cur-col)) )
    (forward-char     (length comment-str))
    (del-ch           (- (length comment-str)) )
    (fwd-line 1)
    (goto (+ (point) col))))
(defvar comment-str ";;")

(defvar my-mode-comment-str-alist
  '()
  ":dm
   what   : assoc list of (my) mode names that map to the corresponding
            appropriate comment marker strings.
  ")
(setq my-mode-comment-str-alist '(
  ("elisp"             . ";;")
  ("clisp"             . ";;")
  ("python"            . "#")
  ("javascript"        . "//")
  ;(""             . "")
))


(provide 'comments)
