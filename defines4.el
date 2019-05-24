;;/  :properties (text)    (2)
                            ;;/  |   general
(defun next-text-prop-range-match (pos tags vals &optional limit)
  (when (not limit)      (! limit  (1- (pt-max))))
  (let*((next-match   (ƛ (i tags vals limit)
                  (let*(vals* R)
                    (while (and  (<= i limit)
                                 (or  (not  (! vals*  (text-prop-vals i tags)))
                                      (not  (equal?   vals  vals*))))
                      (++ i))
                    (if (equal?  vals  vals*)   i   nil))))        
        (next-non-match  (ƛ (i tags vals limit)
                  (let*(vals*)
                    (while (and  (<= i limit)
                                 (! vals*  (text-prop-vals i tags))
                                 (equal?   vals  vals*))
                      (++ i))
                    i))))
    (let*((beg        (⊙ next-match  pos  tags  vals  limit)))
      (if beg 
          ;;/ :dm consider for match till end of buff
          (let*((end   (⊙ next-non-match  beg  tags  vals  limit)))
            `(,beg  .  ,end))
        nil))))
;;/ Usage:
;;    (defun collect-text-prop-range-matches (i target &optional limit)
;;      (let*(r ∑
;;            (tags/vals       (uncollate-plist target))
;;            (tags            (nth 0 tags/vals))
;;            (vals            (nth 1 tags/vals)))
;;        (while   (! r   (next-text-prop-range-match  i  tags vals  limit))
;;          (push  r  ∑)
;;          (! i     (cdr r)))
;;        (nreverse ∑)))
(defun text-prop-vals (i tags)  ;; :dm : for dlm.el ?
  (let*(∑ (ok? t))
    (while (and  tags  ok?)
      (let*((tag              (pop tags))
            (found-tag?       (plist-member   (text-props-at i)   tag)))
        (if found-tag?
            (push    (text-prop i tag)    ∑)
          (! ok?  nil))))
    (if ok?   (nreverse ∑)   nil)))
(defun prop-range (pos prop)                                          ;;/ :NEW  -- GOOD
  (let*((i     (previous-single-property-change (1+ pos) prop))
        (j     (next-single-property-change pos prop)))
    `(,i . ,j)))
(defun prop-ranges (prop &optional beg limit buf)                     ;;/ :new
  "Returns list of recs: (<range> <text-prop-value>)"
  (! buf          (cond
                   ((str? buf)     (get-buf buf))
                   ((buffer? buf)  buf)
                   (t              (cur-buf))))
  (with-buf buf
    (when (not beg)    (! beg   (pt-min)))
    (when (not limit)  (! limit (pt-max)))
    (let*((next-seg  (ƛ (a)
                (if (>= a limit)  nil 
                  (let*((b  (next-1-prop-delta a prop buf limit)))
                    (if b `(,a ,b) `(,a ,limit))))))
          (i    (if beg beg 1))
          ∑)
      (while (! rng   (⊙ next-seg i))
        (let*((val    (text-prop (nth 0 rng) prop)))
          (when val     (push  `(,rng ,val)  ∑))
          (! i  (nth 1 rng))))
      (reverse ∑))))
(defalias 'pseg-ranges        'prop-ranges)
;;/ Usage:
;;          (defun x () (interactive) (/c 2)
;;            (with-buf "out1"
;;              (let*(∑)
;;                (! ∑     (prop-ranges 'field nil nil "out1"))
;;                (dolist (r  ∑)
;;                  (let*((rng      (nth 0 r)))
;;                    (>> 2 "[%s] [%S]\n" r (apply 'buf* rng)))))))

;;|             ------ high(er) level --------- 
(defun hide-range-as (beg end &optional spec)
  (set-range-invisi beg end spec)
  (when  (and (list? buffer-invisibility-spec)
              (not (memq spec buffer-invisibility-spec)))
    (add-buf-invisi-spec spec))  )  ;;/ :dm ???????  placement?
(defalias 'unhide-range  'rem-range-invisi-spec)
;;|             ------ invisi prop ---------
(defun set-range-invisi (beg end &optional spec)
  ;; If SPEC matches an entry in BUFFER-INVISIBILITY-SPEC the
  ;; text in the range will be hidden.
  ;;(put-text-property  beg end 'invisible  (if spec spec t))
  (text-prop!  beg end 'invisible  (if spec spec t))
  )
(defun rem-range-invisi (beg end)
  (remove-text-properties beg end '(invisible any)))
;;|            -------- buf spec -------
(defun add-buf-invisi-spec (spec &optional noredraw)
  (when (or  (not (list? buffer-invisibility-spec))       ;;/ when not orig (reset) state
             (not (memq spec buffer-invisibility-spec)))  ;;/ not already member
    (add-to-invisibility-spec spec)
    (when (not noredraw)  (redraw-display))))
(defun rem-buf-invisi-spec (spec &optional noredraw)
  (when (and (list? buffer-invisibility-spec)
             ;;(not (not (memq spec buffer-invisibility-spec))) ;; :dm MEMQ does not handle all cases
             (member spec buffer-invisibility-spec))
    (remove-from-invisibility-spec spec)
    (when (not noredraw)  (redraw-display))))
(defun reset-buf-invisi-spec (&optional noredraw)
  (! buffer-invisibility-spec t)
  (when (not noredraw)  (redraw-display)))
(defun /rem-read-only () 
  (interactive)
  (when mark-active
      (! inhibit-read-only  t)
      (text-prop! (r-beg) (r-end) 'read-only nil)
      (! inhibit-read-only  nil)))

(provide 'defines4)

