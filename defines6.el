;;/  :emacs

;; ==================================================================
(defun fu (fu-name)
  "insert template interactive function w/o args"
  (interactive
      (list (read-string "function name: " nil nil "x")))
  (let ()
    (insert (format "(defun %s ()" fu-name))
    (if (str=? "x" fu-name)
        (insert " (interactive) ;;(/c)\n")
      (insert "\n  (interactive) ;;(/c)\n"))
    (insert "  (let*()\n")
    (insert "    \n")
    (insert "    ))")
    (forward-line -1)
    (eol)
    ))

(defun fuu (fu-name)
  "insert template for interactive function w/ args"
  (interactive
     (list (read-string "function name: " nil nil "x")))
  (let ()
     (insert 
      (fmt (str+ 
        "(defun %s ()"
        "%s" 
        "  (interactive (list\n"
        "      "
        "(read-string \"input: \" nil nil \"default-val\") \n"
        "      "
        "(compl-read/1 \"input: \"  '(\"a1\" \"a2\"))"
        "    ))\n"
        "  (let*()\n"
        "    \n"
        "    ))")
           fu-name
           (if (str=? "x" fu-name) "" "\n")))
     (forward-line -1)
     (eol)))



;; ==================================================================

(defun bookmark-set* () 
  (interactive)
  (let*((s      (cond 
                 (mark-active      (buf-str-wo (r-beg) (r-end)))
                 (t                (let*((v     (thing-at-point 'symbol)))
                                     (if v (substr-wo v) v)))))
        (name    (cond
                  (s    (read-string "bookmark as: " s nil ""))
                  (t    (read-string "bookmark as: " "" nil "")))))
    (when name  (bookmark-set name 'no-overwrite))))

(defun bookmark-set/view () 
  (interactive)
  (cond
   (mark-active     (bookmark-set*)
                    (deactivate-mark))
   (t               (list-bookmarks)
                    (switch-to-buffer "*Bookmark List*" t))))

(defun narrow? (&optional buf)
  (with-buf  (if buf buf (cur-buf))
    (not (= (pt-max) (1+ (buffer-size))))))

;;| new - allows for elisp comment at bol 
(defun d-def-abrvs () (interactive)
  (when (not mark-active)  (error (fmt ":d - oops - command designed to work with active region")))
  (let*((beg         (r-beg))
        (end         (r-end))
        ∑)
    (deactivate-mark)
    (goto beg)  (bol)    
    (let*((grab-abrv-line  (ƛ () 
               (srch-re-> "[^; ]" (eol-pos))  ;; skip over elisp comment
               (goto (m-beg 0))
               (srch-re-> "\\([().A-Za-z0-9\\-]+\\) +\\([().A-Za-z0-9\\-]+\\)" (eol-pos))
               `(,(m-str 1) ,(m-str 2))  ;; = (abrev text))))
               )))
      (while (< (pt) end)
        (let*((rec   (⊙ grab-abrv-line)))
          (push rec ∑)
          (fwd-line)))
      (! ∑   (nreverse ∑))
      (while (! rec (pop ∑))
        (apply   'define-abbrev  global-abbrev-table   rec)))))
;; old - does not allow for elisp comments
(defun d-def-abrvs () (interactive)
  (when (not mark-active)  (error (fmt ":d - oops - command designed to work with active region")))
  (let*((beg         (r-beg))
        (end         (r-end))
        ∑)
    (deactivate-mark)
    (goto beg)  (bol)
    (while (< (pt) end)
      (srch-re-> " *\\([^ ]+\\) +")
        (let*((abrv      (substr-wo (m-str 1)))
              (txt       (buf-str-wo (pt) (eol-pos))))
          (push `(,abrv ,txt) ∑)
          (fwd-line)))
    (! ∑   (nreverse ∑))
    (let*(rec)
      (while (! rec (pop ∑))
        (apply   'define-abbrev  global-abbrev-table   rec)))))

;;/  :list     (2)
(defun dotted-pair? (v)
  (and   (list? v)   (not (list? (cdr v)))))
(defun findf (want? lst)
  (let*(e found?)
    (while (and  lst  (not found?))
      (! e   (pop lst))
      (when (⊙ want? e)
        (! found? t)))
    (if found? e nil)))

(defun mapcar* (function &rest args)
  ;;/ :dm from: '(elisp) Mapping Functions'  (just re-formated for readability)
  ;;/ This is copied from elisp doc ... I believe it is problematic in that
  ;;/ it relies on a recusive call -- greatly limiting the size?
  "Apply FUNCTION to successive cars of all ARGS.
   Return the list of results."
  (if (not (memq nil args))                        ;;/ If no list is exhausted,
      (cons 
         (apply function (mapcar 'car args))       ;;/ apply function to CARs.
         (apply 'mapcar* function                  ;;/ Recurse for rest of elements.
                (mapcar 'cdr args)))))
;;/ :dm ‶my″ alternative the the MAPCAR* example above -- not sure which is better
;;(defun mapcar* (fu &rest args)
;;  (let*((more?   (ƛ (args)  (catch 'found
;;                              (mapc  (ƛ (a)(when a (throw 'found t)))  args)
;;                              nil)))
;;        results)
;;    (while (⊙ more? args)
;;      (let*((fu-args     (mapcar 'car args)))
;;        (! args (mapcar 'cdr args))
;;        (push  (apply fu fu-args)  results)))
;;    (reverse results)))

;;/ :dm (for ref) a re-factoring of MAPCAR*
;;(defun mapcar* (function &rest args)
;;  (when (not (memq nil args))
;;    (let*((r1       (apply function (mapcar 'car args)))
;;          (r2       (apply 'mapcar* function
;;                           (mapcar 'cdr args))))
;;      (cons r1 r2))))
(defun andmap (function &rest args)
    (when (not (memq nil args))                            ;;/ If no list is exhausted,
        (let*((results        (apply function (mapcar 'car args))))
          (if results
            (let*((remainder    (mapcar 'cdr args)))
              (if (memq nil remainder)                     ;;/ last-level?
                  (apply function (mapcar 'car args))
                (apply 'andmap function remainder)))       ;;/ Recurse for rest of elements.
              nil))))                                      ;;/ 1 nil results => ANDMAP returns nil
;;(defun x () (interactive)    ;;/ ANDMAP devl
;;  (let*((lst1   '(1 2 3))
;;        (lst2   '(4 5 6)))
;;    (p (fmt "[%S]" 
;;            (andmap 
;;               (ƛ (a b)
;;                  (let*((v   (+ a b)))
;;                    (msg> (fmt "[%S] [%S] [%S]" a b v))
;;                    (if (>  v  4)   `(,a ,b ,v yes)    nil)))
;;             lst1 lst2)))))
(defun ormap (function &rest args)  ;;/ good  +  IMPORTANT KEY TO UNDERSTANDING RECURSION
  (when (not (memq nil args))
    (let*((results       (apply function (mapcar 'car args))))
      (if results
          results
        (let*((ret     (apply  'ormap  function   (mapcar 'cdr args))))
          ret)))))
;;(defun x () (interactive)   ;;/ ORMAP devl
;;  (let*((lst1   '(1 2 3))
;;        (lst2   '(4 5 6)))
;;    (p (fmt "[%S]" 
;;            (ormap 
;;               (ƛ (a b)
;;                  (let*((v   (+ a b)))
;;                    (if (>  v  55)  `(,a ,b ,v yes)   nil)))
;;               lst1 lst2)))))
(defun list->vec (lst) 
  (let*((v     (make-vec (len lst) nil)))
    (dotimes (i  (len lst))  (aset v i (pop lst)))
    v))
;;/ Usage:
;;  (list->vec 
;;   `(,(ƛ () (buffer-face-set `(:height 100)))
;;     ,(ƛ () (buffer-face-set `(:height 90)))
;;     ,(ƛ () (buffer-face-set `(:height 80))) ))

;;/  :hash
(defun hashkeys (h)
  (let*( lst )
    (maphash (ƛ (k v) (<- lst (cons k lst))) h)
    lst))
(defalias '⋕keys         'hashkeys)
(defun pushhash (k v h)
    (puthash k (cons v (gethash k  h)) h))
(defalias  'push⋕ 'pushhash)
(defun hashvals (h)                              ;;/ :new
  (let*(∑)
    (map⋕ (ƛ (k v)  (push v ∑))   h)
    ∑))

;;/  :vector   (2)
(defun subvec (v b e)
  (let*((sz       (- e b))
        (v*       (make-vector sz t))
        (i        0))    
    (while (< i sz)
      (let*((e     (aref v (+ i b))))
        (aset v* i  e)
        (! i  (+ 1 i))))
    v*))

;;/  :alist     (2)
;;/ :dm - ALIST-ASSIGNMENTS - should be changed ... probably better
;;/ (more efficient) to convert NEW-ITEMS into a hash rather 
(defun alist-assignments (alist &optional new-items)
  (let*(e  ∑)
    (while (! e  (pop alist))
      (if (list? e)
          (let*((repl    (assq (car e) new-items)))
            (cond
               (repl   (push repl ∑)
                       (! new-items    (remove repl new-items)))
               (t      (push e ∑))))
        (push e ∑)))
    (while (! e   (pop new-items))
      (push e ∑))
    (reverse ∑)))
(defun tget. (tag lst)
  (let*((r    (tget tag lst)))
    (if r  (cdr r)  nil)))
(defun tagged-as? (tag dat)  ;;/ YES
    (and  (list? dat)   (not (null dat))  (eq? tag (car dat))))

;; ==================================================================

;; ==================================================================
;;/  my formated files  (2)

(defun /gather-code-clip-2-clipboard () 
  (interactive)
  ;;/ Selected region is used as input for match replacment of unicode 
  ;;/ quote marks ‶″ w/ the standard ascii quote mark. The results are
  ;;/ put into the X Window's clipboard.
  ;;/ Motivation: code in my tag-formated text that has the quote marks
  ;;/ w/in the code replace w/ unicode for compatbility reasons can be
  ;;/ easily copy/pasted for use as actual code.
    (if mark-active
        (let*()              
          (x-set-selection 'CLIPBOARD 
                (replace-regexp-in-string "″" "\""
                     (replace-regexp-in-string "‶" "\"" (buf-str-wo (r-beg) (r-end)))))
          (deactivate-mark))
      (error (fmt ":d - oops - not mark-active"))))


;; ==================================================================
;;/  :aux

(defun fu-sig->clipboard (func)
  (let*(func/str)
    (cond
     ((sym? func)       (! func/str   (sym->str func)))
     ((str? func)       (! func/str   func)
                        (! func       (str->sym func)))
     (t                 (error (fmt ":d - oops"))))
    (save-window-excursion
      (describe-function func)
      (other-window 1)
      (when (not  (srch-re->  (str+ "("  func/str  "[^)]*"  ")") nil t))
        (error (fmt ":d - regexp match of signature failed")))
      (x-set-selection 'CLIPBOARD (m-str 0)))))

;; ==================================================================

(global-font-lock-mode t)
;; highlight "marked" area (as 'Region face')
(transient-mark-mode t)  

(put 'narrow-to-region 'disabled nil)
;(hi-lock-mode t)
(global-hi-lock-mode 1)
;(setq hi-lock-file-patterns-policy t)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; ==================================================================

(defun initialize-for-scheme-editing () ;; called fromo 'scheme-mode-hook
  (interactive)
  (let*( found? end )
    (save-excursion
      (goto (point-min))
      (setq found?    (srch-re->  (str+ ";;/ "  "local aux") nil t))
      (when found?
        (bol)
        (forward-line        -1)
        (if mark-active      (deactivate-mark))
        (set-mark (point))
        (setq end            (point))
        (goto                (point-min))
        (narrow-to-region    (point) end)
        (deactivate-mark)))))


;; ==================================================================

;;/ :dlm -- FILTER conflects with a previous define below                  FIX THIS
;;/  the one blew had the args in reverse order from this
(defun filter (want? lst)
  (let*(∑)
    (mapc  (ƛ (e) (when (call want? e)  (push e ∑)))
           lst)
    (reverse ∑)))
(defalias 'filt          'filter)
(defun filter-not (not-want? lst)
  (let*(∑)
    (mapc  (ƛ (e) (when (not (call not-want? e))  (push e ∑)))
           lst)
    (reverse ∑)))
(defalias 'filt-not      'filter-not)
(defun filter-map (fu lst)
  (let*(∑)
    (mapc   (ƛ (e) (let*((r    (call fu e)))
                     (when r  (push r ∑))))
            lst)
    (reverse ∑)))
(defalias 'filt-map 'filter-map)


;; ==================================================================
(provide 'defines6)
