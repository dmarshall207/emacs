
(defun >> (f &rest v)
  (let*()
	 (with-buf "out"
	   (goto (pt-max))
	   (ins (apply 'fmt f v))
	   )
    ))



(defun x () (interactive) ;;(/c)
       (let*((fr               (open-bare-frame "out" ))
             (pars            (frame-parameters fr))
             )
         (! pars          `(,@pars  (test . 100)))
         (modify-frame-parameters fr pars)
         (prin1 (fmt "r [%s]\n" fr))
         (prin1 (fmt "r- [%s]\n" (aget 'test (frame-parameters fr))))
         
         ))

(defun x () (interactive) ;;(/c)
       (let*((r           '((a . 1) )))
         (! r      `(,@r (b . 2)))
         (prin1 (fmt "[%s]\n"  (aget 'b r)))
         
         
    ))


