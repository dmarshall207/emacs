;; plate-util-setup.el

(require 'plate-util3 "./plate/plate-util3.el")
(defvar *my-mode-plate-dir-alist*  (list
  ;;| note: the new subdir name convetion for the new plate format
  ;;| is 'plate.as-dat'
  ;;(cons    "elisp"         "/sto/elisp-plate.as-dat")				    
  ;;(cons    "scheme"        "/sto/copies-from-zippy/scheme/plate.as-dat")				    
 `("elisp"        . ,(getenv "emacs_plate_dir_path"))
 `("scheme"       . ,(getenv "scheme_plate_dir_path"))
  )
;  ":dm
;   what   : assoc list of (my) mode names that map to the corresponding 
;            template dir paths. 
;            WARNING: must restart Emacs to see changes.
;  "
  )


(defvar plate-names nil "")
(setq plate-names nil)
(dolist (r *my-mode-plate-dir-alist*)
  (setq  plate-names  (cons (car r) plate-names)))

(defcustom *current-plate-root* "elisp" "/sto/copies-from-zippy/scheme/plate.as-dat")
(defvar *plate-dir*
     (agetv  *current-plate-root*   *my-mode-plate-dir-alist*)
     "")


;; :old
;;(setq *my-mode-plate-dir-alist* '(
;;  ("elisp"        . "/home/proj2/elisp/plate")
;;  ("clisp"        . "/home/proj2/clisp/plate")
;;  ("python"       . "/home/proj2/py/plate2")
;;  ("javascript"   . "/home/proj2/js/plate")
;;  ("xml"          . "/home/proj2/xml/plate")
;;  ("html"         . "/home/proj2/html")
;;  ("xslt"         . "/home/proj2/xslt/plate")
;;  ("red"          . "/home/proj2/red/plate")
;;  ;(""             . "")
;;))


(provide 'plate-util-setup)
