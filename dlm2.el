
;; ᵈhi-lock-hash
;; ᵈhi-on
;; ᵈhi-off
;; deactivate-hi-by-name
;; activate-hi-by-name
;; ᵈhi-lock-hash/put-entry
;; normalized-hi-lock-spec
;; mark-rec
;; ᵈins-@

(defun pick-list ( lst &optional prompt init)
    (when (not prompt)  (<- prompt "pick: "))
    (completing-read  prompt
          lst       ;; collection
          nil       ;; predicate
          2         ;; require-match (nil t confirm confirm-after-completion other)
          init      ;; initial
          'lst))    ;; hist

;;|--------------------------------------------------------------------------
;;| <section>
;;/  Logic providing user ability to turn/on off regexp controled highlighting
(defvar ᵈhi-lock-hash           (make-hash-table :test 'equal)
  "Key   : name  (string)
   Value : list of hi-lock specs
   Provides the data for logic to turn on/off a set of named hi-lock
   entries")

;;(defun ᵈhi-on () ;;/ changing this so I can order the keys
;;  (interactive)
;;  (let*( (s  (pick-list (hashkeys ᵈhi-lock-hash) "selecte hi name: ")))
;;    (activate-hi-by-name s)))
(defun ᵈhi-on ()
  (interactive)
  (let*( (s  (pick-list 
              '(";h" ":dm""\":" )
              "selecte hi name: ")))
    (activate-hi-by-name s)))




(defun ᵈhi-off ()
  (interactive)
  (let*( (s          (pick-list (hashkeys ᵈhi-lock-hash) "selecte hi name: ")) )
    (deactivate-hi-by-name s)))

(defun deactivate-hi-by-name (name)
  (let*((spec-lst     (gethash  name  ᵈhi-lock-hash)))
    (cond
     (spec-lst  
            (dolist (s  spec-lst)
                (font-lock-remove-keywords  nil  `(,s)))
            (font-lock-fontify-block  80))
     (t  (p (fmt "err: hi-hash entry not found : %S" name))))))

(defun activate-hi-by-name (name)
  (let*((spec-lst     (gethash  name  ᵈhi-lock-hash)))
    (cond
     (spec-lst
            (deactivate-hi-by-name  name)
            (dolist (s  spec-lst)
                (font-lock-add-keywords   nil  `(,s)))
            (font-lock-fontify-block  80))
     (t  (p (fmt "err: hi-hash entry not found : %S" name))))))

(defun ᵈhi-lock-hash/put-entry (name spec-lst)
  (remhash name ᵈhi-lock-hash)
  (dolist (s  spec-lst)
      (<- s  (normalized-hi-lock-spec s))
      (pushhash name s ᵈhi-lock-hash)))

(defun normalized-hi-lock-spec (lst)
  ;;/ :dm - ? internalize to ᵈHI-LOCK-HASH/PUT-ENTRY
  ;; Takes LST which is one of my hi-lock specs and turns
  ;; it into something useable for FONT-LOCK-ADD-KEYWORDS
  ;; note: at this point all it really does is allow me 
  ;; to leave out the regexp group number (by adding a '0').
  ;; Most of if is just error checking.
  ;; Example input   :  ("test"     'stand-out-green  prepend)
  ;; returns         :  ("test"  0  'stand-out-green  prepend)
  (let*(e  (i 0)  ∑ quoted-face?)
    (<- quoted-face? 
               (ƛ (itm)
                  (and (list? itm)
                       (= 2 (len itm))
                       (quote? (car itm))
                       (not (not (memq (nth 1 itm) (face-list)))))))
    (while lst
      (let*((itm   (car lst)))
        (cond
         ((= 0 i)          ;;|  0  =>  regexp
                ;;(call process-0 itm i)
                (cond ((str? itm)   (push itm ∑)
                                    (<- i  (1+ i))
                                    (pop lst))
                      (t            (error "#0 itm not a string"))))
         ((= 1 i)           ;;|  1 =>  regex group #
                (cond
                    ((int? itm) 
                                    (push itm ∑)
                                    (<- i  (1+ i))
                                    (pop lst))
                    ((and  (not (int? itm))  (call quoted-face? itm))
                                    (push 0 ∑)
                                    (<- i  (1+ i)))
                    (t              (error (fmt "not a quoted face spec [%S]" itm)))))
         ((= 2 i)          ;;|  2 => face spec
                   (cond
                      ((not (call quoted-face? itm))
                                    (error "#2 is not a quoted face"))
                      (t            (push itm ∑)
                                    (<- i  (1+ i))
                                    (pop lst))))
         ((= 3 i)          ;;|  2 =>   'prepend'  or 'append'
                   (cond
                       ((memq itm  `(prepend append))
                                    (push itm ∑)
                                    (<- i  (1+ i))
                                    (pop lst))
                       (t           (error "#3 not a valid flag"))))
         ((> i 3)  (error "too many items in spec list")))))
    (reverse ∑)))


;;/ initialize  ᵈHI-LOCK-HASH
(let*((entries `(("@"     ("@ *[0-9:]+"     'stand-out-green  prepend))
                 (";h"    (";h"             'stand-out-red  prepend))
                 (":dm"   (":dm"            'stand-out-green  prepend))
                 ("\":"   ("\": +\\(.*\\)" 1   'fg-goldenrod3  prepend))
                 )))
          (dolist (e  (reverse entries))
              (ᵈhi-lock-hash/put-entry  
                   (car e)       ;; name
                   (cdr e))))

;;(defun initialize-ᵈhi-lock-hash ()
;;  (interactive)
;;        (let*((entries `(("@"     ("@ *[0-9:]+"    'stand-out-green  prepend))
;;                         )))
;;          (dolist (e  entries)
;;              (ᵈhi-lock-hash/put-entry  
;;                   (car e)       ;; name
;;                   (cdr e)))))   ;; spec list
;;(initialize-ᵈhi-lock-hash)
;;| </section>

(defun mark-rec ()
  ;; :dm - consider  replacing  MARK-ACTIVE w/ USE-REGION-P
  (let*()
    (cond (mark-active   `((:start . ,(r-start))
                           (:end   . ,(r-end))
                           (:text  . ,(buf-substr-wo (r-start) (r-end)))))
          (t             nil))))

(defun ᵈins-@ ()
  (let*()
    (insert "((@ :)
\"\")\n")
    (forward-line -2)
    (forward-char 4)
    ))

(provide 'dlm2)
