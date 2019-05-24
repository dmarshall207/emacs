

;;/ :dm - err - the plate dir is hard coded


;------------------------------------------------------------------
(defun d-select-plate-root ()
  (interactive)
  (let* (r items name name-list)
    (dolist (r *my-mode-plate-dir-alist*)
          (push (car r) name-list))
    (setq name   (read-from-options-list "plate root name : " name-list ) )
    ;: add check for valid name
    (setq *current-plate-root*  name)
    (setq *plate-dir* 
          (agetv  *current-plate-root*   *my-mode-plate-dir-alist*))
)) 

;------------------------------------------------------------------
(defun d-set-plate-root (plate-name)
  (cond
    ((eq nil (assoc  plate-name  *my-mode-plate-dir-alist*))
          (error (format "ERR: no such plate name `%s'" plate-name)))
    (t
          (setq *current-plate-root*  plate-name)
          (setq *plate-dir* 
                (agetv  *current-plate-root*   *my-mode-plate-dir-alist*))))
)


(defun d-insert-plate3 () (interactive)
  (let*(;;-b--------------------------------------------------
        (insert-plate (ƛ (path)
          (let*((get/open-plate-buf  (ƛ (path)
                          (let*((lst    (filt  (ƛ (r) (str=? path (ᴾget :path r)))  (open-file-recs2)))
                                (open?  (= 1 (len lst)))
                                (err?   (> (len lst) 1)))
                            (cond 
                             (err?                (error (fmt "wtf")))
                             (open?               (! close?   nil)
                                                  (ᴾget :buf (car lst)))
                             (t                   (! close?   t)
                                                  (find-file-noselect path))))))
                (read-dat (ƛ ()
                          (with-buf buf
                              (goto 1)
                              (read  buf))))
                (tref-drill (ƛ (tlist tpath)                      ;;/ :dm  nice
                          (let*(tag)
                            (while (! tag (pop tpath))
                              (! tlist    (tref tag tlist)))
                            tlist)))
                (get-subsitution-id/vals  (ƛ ()
                          ;; prompts the user for values to use in plate subsitutions
                          (let*((substitutions      (cdr   (⊙ tref-drill dat '(:plate :substitutions))))
                                (delimiter-spec?    (ƛ (e) (and (list? e)  (not (null e)) (eq? ':delimiter (car e)))))
                                e  ∑)
                            (if substitutions
                                (let*((delim    (pop substitutions)))
                                  (! delimiter   (nth 1 delim))
                                  (when (not  (⊙ delimiter-spec? delim))  (error (fmt "oops - missing delimeter spec.")))
                                  (while (! e  (pop substitutions))
                                    (let*((id      (ᴾget :id e))
                                          (res     (read-string  (ᴾget :prompt e))))
                                      (push  `(:id ,id :val ,res)  ∑)))
                                  ∑)
                                nil))))
                (expand-plate-text (ƛ (txt) 
                    ;; note: peforms substitutions
                    (let*((sub-recs       (⊙ get-subsitution-id/vals))
                          r)
                      (while (! r   (pop sub-recs))
                        (let*((id         (ᴾget :id  r))
                              (val        (ᴾget :val r))
                              (from/str   (fmt "%s%s%s" delimiter id delimiter))
                              (from/re    (regexp-quote from/str)))
                          (! txt       (replace-regexp-in-string  from/re  val  txt))))
                       txt)))
                buf close? dat txt delimiter insert-beg insert-end)
            (! buf      (⊙ get/open-plate-buf path))
            ;;(error (fmt "oops - 1"))

            (! dat      (⊙ read-dat))
            ;;/ :dm - there does not  seam to be any  POST-EXPANSION-CODE  ... skiping for now
            (let*((txt           (⊙ expand-plate-text
                                         (nth 1  (⊙ tref-drill dat '(:plate :plate-text))))))
              (cond 
               (mark-active        ;;/ ---- selected text -----
                      (let*((core-mark?     (re-str-match (regexp-quote ":core:") txt)))
                        (let*((core-txt      (buf-str (r-beg) (r-end))))
                          (when core-mark?  (del-region (r-beg) (r-end)))
                          (! txt    (replace-regexp-in-string 
                                     (regexp-quote ":core:") core-txt  txt)))))
               (t                  ;;/ ---- no selected text ----
                     (let*((core-mark?     (re-str-match (regexp-quote ":core:") txt)))
                       (when core-mark? 
                         (! txt    (replace-regexp-in-string 
                                    (regexp-quote ":core:") ""  txt))))))
              (! insert-beg    (point))
              (insert-at-current-column txt)
              (! insert-end    (point)))
            (let*((post-ins-code    (⊙ tref-drill dat '(:plate :post-insertion-code)))
                  (post-ins-fu      (nth 1 post-ins-code)))
              (when post-ins-fu   (⊙ post-ins-fu  insert-beg  insert-end)))
            (when close?  (kill-buf  buf)))))

        ;;-e--------------------------------------------------

        ;;(plate-dir     "/sto/scheme/plate.as-dat")
        (plate-dir       *plate-dir*)
        
        (plate-list      (filt-map  (ƛ (s)  (let*((i  (re-str-match "\\._$" s)))
                                              (if i  (substr s 0 i)   nil)))
                                    (ᴾget :files (dir-listing plate-dir))))
        (name       (read-from-options-list "plate : " plate-list ))
        ;;(path       (fmt "%s%s%s.plate" plate-dir *path-delimiter* name))
        (path       (fmt "%s%s%s._" plate-dir *path-delimiter* name))
        )
    (⊙ insert-plate path)
    ))

(provide 'plate-util3)
