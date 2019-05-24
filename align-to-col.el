
;; This is copied from
;;  * /sto/copies-from-zippy/scheme/devl.emacs-helper/emacs-code/align-to-col.el


(defun d-align-to-col ()
  ;; (:mri  1MdF7JhZA)
  (interactive)
  (when (not (bound? 'align-to-col-target))
    (error (fmt ":d - oops - ALIGN-TO-COL-TARGET has not been set")))
  (let*( (target    align-to-col-target)
         (c-col     (cur-col))
         (prev-nonblank-col
                    (ƛ () (save-excursion
                            (let*((m   (srch-re-< "[^ ]" (bol-pos) t)))
                              (cond
                               (m     (cur-col))
                               (t     (bol)
                                      (cur-col))))))))
     (cond
         ((> target  c-col)
               (insert   (make-string (- target c-col) ? )))
         ((< target  c-col)
               (backward-delete-char-untabify 
                (min  (1- (- c-col (call prev-nonblank-col)))
                      (- c-col target)))))))

;;/ orig -- has bug -- fix in 1MdF7JhZA
;;(defun d-align-to-col ()
;;  (interactive)
;;  (when (not (bound? 'align-to-col-target))
;;    (error (fmt ":d - oops - ALIGN-TO-COL-TARGET has not been set")))
;;  (let*( (target    align-to-col-target)
;;         (c-col     (cur-col))
;;         (prev-nonblank-col
;;                    (ƛ () (save-excursion
;;                            (srch-re-< "[^ ]" (bol-pos))
;;                            (setq  col  (cur-col))))))
;;     (cond
;;         ((> target  c-col)
;;               (insert   (make-string (- target c-col) ? )))
;;         ((< target  c-col)      
;;               (backward-delete-char-untabify 
;;                (min  (1- (- c-col (call prev-nonblank-col)))
;;                      (- c-col target)))))))

(defvar align-to-col-target nil)

(defun d-set-to-col ()
  (interactive)
  (setq align-to-col-target  (cur-col)))

;; ORIG
;;(global-set-key (kbd "C-z") 'd-set-to-col)
;;;;(global-set-key (kbd "M-z") 'd-align-to-col)
;;(global-set-key (kbd "C-a") 'd-align-to-col)



(provide 'align-to-col)
