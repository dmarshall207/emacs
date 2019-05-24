;; ==================================================================
;;/  :number

(defmacro ++ (var)
  (list 'setq var (list '1+ var)))

(defmacro -- (var)
  (list 'setq var (list '1- var)))

;; ==================================================================
;;/  :string    (2)

(defun rem-str-props (s)
  "Remove text properties from string S." 
  (set-text-properties 0 (len s) nil s)
  s)

(defun str-pad/right (s amt)
  (let*((n      (if (> (len s) amt)  0   (- amt  (len s)))))
    (str+  s  (make-str n ? ))))

(defalias 'str-pad/r   'str-pad/right)

(defun str-pad/left (s amt)
  (let*((n      (if (> (len s) amt)  0   (- amt  (len s)))))
    (str+  (make-str n ? )  s)))

(defalias 'str-pad/l   'str-pad/left)

;;(defun str-join (sep &rest args)
;;  (let*((cnt     (len args))
;;        e ∑)
;;    (while  (! e   (pop args))
;;      (-- cnt)
;;      (! ∑    (if (> cnt 0)  `(,sep ,e  ,@∑ )  `(,e ,@∑))))
;;    (apply 'str+ (reverse ∑))))

(defun str-join2 (sep &rest args)
  (let*((cnt     (len args))
        e ∑)
    (while  (! e   (pop args))
      (-- cnt)
      (! ∑    (if (> cnt 0)  `(,sep ,e  ,@∑ )  `(,e ,@∑))))
    (apply 'str+ (reverse ∑))))

;; ==================================================================
;;/  :regexp       (2)

(defun m-ranges (&optional m-data)
  ;; note: assumes m-data is returned by MATCH-DATA call w/ the
  ;; INTEGERS arg set to true (i.e. returns positions rather than marks
  ;; and last elm is the buffer.
  (when (not m-data)        (! m-data   (match-data t)))
  (let*(∑)
    (while (> (len m-data) 1)
      (push   `(,(pop m-data)  .  ,(pop m-data))    ∑))
    (nreverse ∑)))

(provide 'defines7)
