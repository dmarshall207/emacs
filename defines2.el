;;/  :plist              (2)
;;(defun pput! (pl k v)      (set pl  (plist-put (sym-val pl) k v)))
(defmacro pput! (pl k v)  `(setq ,pl (plist-put ,pl ,k ,v)))   ;;/ :dm ???? not needed ????
;;(defalias 'ᴾget           'plist-get)
;;(defalias 'ᴾput           'plist-put)
(defalias 'ᴾkey?          'plist-member)
(defmacro ᴾget (prop plist)         `(plist-get ,plist ,prop))
(defmacro ᴾput (prop val plist)     `(plist-put ,plist ,prop ,val))
(defun ᴾremove (target plst)
  (let*(found? head)
    (while (and  plst  (not found?))
      (let*((k   (pop plst))
            (v   (pop plst)))
        (if (eq? target k)
            (! found? t)
          (! head   `(,@head ,k ,v)))))
    `(,@head ,@plst)))
(defalias 'prmv            'ᴾremove)
(defalias 'sym-plist       'symbol-plist)

(defun rmv (s p)          ;;/ :NEW
  ;;. Removes property P from symbols S's plist.
  (let*(∑
        (plst    (sym-plist s))
        (found?   (catch 'found
                    (let*(kv)
                      (while (! kv   (take plst 2))
                        (when (eq? p (car kv))  (throw 'found t))
                        (! ∑  `(,@kv ,@∑))
                        (! plst  (drop plst 2)))))))
    (when found?
      (setplist s  `(,@(reverse ∑)  ,@(drop plst 2))))))


;;/  :tlist              (2)
(defun tlist->plist (tlst &optional translate? default noerr?)   ;;/ :NEW
    (let*(e ∑)
      (while tlst
        (let*((e  (pop tlst)))
          (cond
           ((and (list? e) (not (null e)))
                   (push  (car e)  ∑)
                   (push  (cdr e)  ∑))
           (translate?                 ;; include non-list as key/default 
                   (push  e        ∑)
                   (push  default  ∑))
           ((and (not translate?) noerr?)
                  'do-nothing)
           (t    (error (fmt ":d - TLIST->PLIST - bad TLST"))))))
      (! ∑  (nreverse ∑))))


;;<sec>
;;/  :pseg  (property-segment)
(defun pseg-at (i p)
  "Returns a rec with info about the PSEG at I based on property P"
  ;;| left-edge is ‶range inclusive″ right-edge is ‶range exclusive″
  (let*((bob?       (= i (pt-min)))
        (eob?       (= i (pt-max)))
        (le?        (pseg-edge/left? i p))
        (re?        (pseg-edge/right? i p))   ;;| :KEY : not inside of what's on the L - may be in the R
        (lv         (if bob? nil (prop (- i 1) p)))   ;; val of P left of I
        (rv         (prop i p))                       ;; val of P right of I
        (b          (cond
                     (le?                             i)
                     ((and (not lv) rv)               i)
                     ((and lv rv (not (eq? lv rv)))   i) ;; ? same as le? -- NO -- ? is a subset of it though
                     ((and lv rv (eq? lv rv))         (prev-1-prop-delta i p))))
        (e           (if rv (next-1-prop-delta i p) nil))
        (str         (if (and b e) (buf* b e) nil)))
    ;;`(,(prop i p) (,b ,e) ,str)
    ;;(if lv `(,lv (,b ,e) ,str)  nil)  ;;/ orig
    (if rv `(,rv (,b ,e) ,str)  nil)))
(defun pseg-edge/right? (i prop)
  (let*((bob?       (= i (pt-min)))
        (r-val      (prop i prop))
        (l-val      (if bob? nil (prop (- i 1) prop))))
    (if (not l-val)  nil
      (cond                       ;;/ => left of point is a char w/ PROP
       ((not r-val)               t)
       ((not (eq? l-val r-val))   t)
       ((eq? l-val r-val)         nil)
       (t                         (error (fmt "wtf RIGTH-EDGE?")))))))
(defun pseg-edge/left? (i prop)
  (let*((bob?       (= i (pt-min)))
        (r-val      (prop i prop))
        (l-val      (if bob? nil (prop (- i 1) prop))))
    (if (not r-val)  nil
      (cond
       ((not l-val)               t)
       ((not (eq? l-val r-val))   t)
       ((eq? l-val r-val)         nil)
       (t                         (error (fmt "wtf LEFT-EDGE?")))))))
;;</sec>

;;/ :dm - this will potentially conflect w/ my earlyer ver of PPUT (macro)
;;/ soooo I will change all the previous uses of PPUT to PPUT* and rename
;;/ the macro to PPUT*
;;/ THIS new ver of PPUT  is guarented to NOT be destructive where as the
;;/ PLIST-PUT can be (see the doc) --- the old ver (now PPUT*) was guarented
;;/ to be destructive
(defun pput  (pl k v)     (plist-put (copy-sequence pl) k v))

;; Function: plist-get  plist  property
(defalias 'pget    'plist-get)

(defmacro pput* (plist k v)  ;;/ :dm new name to prevent conflect (see above)
  `(setq ,plist (plist-put ,plist ,k ,v)))
;;(defmacro pput (plist key value)
;;  `(setq ,plist (plist-put ,plist ,key ,value)))




(provide 'defines2)
