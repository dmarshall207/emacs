;;(global-set-key (kbd "C->")       'indent-code-rigidly)
;;(set-mark p1)


;; DAVIS  :  * change names 'definesN.el' to ? 'zippyN.el'
;;           * for new work --  ? 'ubuN' 


   :beg    x
        111
           222
      :end

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

(global-set-key (kbd "<M-s-right>")     'shift-right)   ;; 'M' = window key
(global-set-key (kbd "<M-s-left>")      'shift-left)
