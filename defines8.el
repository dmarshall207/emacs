;; ==================================================================
;;/  :user-interacton

(defun read-func-name (name)    ;;/ read-func-name
":d Read and return a function name.\n
Motivation: for use with CALL-INTERACTIVELY to read a
function name from the user with completing reads."
  (interactive "aFunction: ")
    name)

(defun grab-func-signature () 
  (interactive)
  ;; :dm  the reason I don't use DOCUMENTATION : returned str by  does not alway provide a signature
  (let*((str    (thing-at-point 'symbol))
        (sym    (if str (str->sym str) nil))
        s)
    (cond
     ((and sym (fbound? sym))
          (fu-sig->clipboard sym)
          (p (fmt ":d - %s  : signature in clipboard" (upcase str) )))
     (t 
          (! sym         (call-interactively 'read-func-name))
          (fu-sig->clipboard  sym)
          (p (fmt ":d - %s  : signature in clipboard" (upcase (sym->str sym)) ))))))


(defvar empty-hist nil 
  ":d Used w/ COMPLETING-READ for an empty history list.
The using code should set to nil after COMPLETING-READ
runs in order to keep it empty.")

(defun compl-read/1 (prompt collection &optional predicate require-match initial-input def inherit-input-method)
  ":d My front-end for COMPLETING-READ that uses an empty history list."
  (prog1 
      (completing-read 
          prompt  collection  predicate  require-match    initial-input 
          'empty-hist  def   inherit-input-method)
    (! empty-hist nil)))

(defun make/install-prefix-keymap (prefix-key prompt &rest specs)
  ":d Makes a keymap from SPECS and installs it in the global map as under prefix-key"
  (let*((keymap+       (ƛ (kmap  &rest specs)
                          (mapc  (ƛ (e) (define-key kmap (car e) (cadr e)))  specs)))
        (make-keymap  (ƛ (prompt &rest specs)
                         (let*((kmap     (make-sparse-keymap prompt)))
                           (apply keymap+ kmap specs)
                           kmap))))
    (let*(prefix-map)
      (! prefix-map     (apply make-keymap prompt specs))
      (define-key   (current-global-map)  prefix-key    prefix-map))))


(defun d-C-z-config (map-id)
  ":d User is prompted to select from a set of pre configured prefix keymaps
specified by an MAP-ID. The assocated keymap is then installed as the 
prefix C-z keymap"
  (interactive (list
       (compl-read/1 "C-z map id:  "  
           '("map 1" "map 2")
           nil  t  "map 1" )))
  (let*((map-1 (ƛ ()
             (make/install-prefix-keymap "\C-z"
                  "prompt Z"
                  `("s" grab-func-signature))))
        (map-2 (ƛ ()
             (make/install-prefix-keymap "\C-z"
                  "prompt Z"))))
    (cond
     ((str=? "map 1" map-id)   (call map-1) )
     ((str=? "map 2" map-id)   (call map-2))
     ((str=? "" map-id)        'no-action)
     (t    (error (fmt ":d - oops no such map-id"))))))



(provide 'defines8)
