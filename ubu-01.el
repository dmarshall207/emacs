(defun shift-right (beg end &optional amt)
  (interactive "r")
  (unless  amt  (! amt 1))                ;; default amount
  (cond
   (mark-active
           (let*((deactivate-mark   nil))      ;; BINGO : https://www.gnu.org/software/emacs/manual/html_node/elisp/The-Mark.html
             (indent-code-rigidly beg end amt)))
   (t
           (prin1 (fmt "[mark not active]" )))))

(defun shift-left (beg end &optional amt)
  (interactive "r") 
  (unless  amt  (! amt -1))                ;; default amount
  (cond
   (mark-active
           (let*((deactivate-mark   nil))        ;; BINGO : https://www.gnu.org/software/emacs/manual/html_node/elisp/The-Mark.html
             (indent-code-rigidly beg end amt)))
   (t
           (prin1 (fmt "[mark not active]" )))))




(provide 'my-edit-tools)
