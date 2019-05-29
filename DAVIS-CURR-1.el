;;(setq lexical-binding t)   ;; now set in 'emacs-startup.el'
;;/ :dm  -   convention:
;;/   bare frames will always have a buffer named the same as the frame-name

(setq lexical-binding t)   ;; ????????????
(open-bare-frame "out" )

(defun x () (interactive) ;;(/c)
  (let*()
    (prin1 (fmt "[%s]\n" lexical-binding))
    (prin1 (fmt "[%s]\n" (default-value lexical-binding)))
    ))

(defun c/frame-append (name)
  (let*((f        (lambda (f &rest v)
                    (with-buf name
                      (goto (pt-max))
                      (insert (apply 'fmt f v))))))
    f
    ))

;;(! out>  (c/frame-append "out"))

(defun x () (interactive) ;;(/c)
       ;;(open-bare-frame "out" )
       (let*((out>  (c/frame-append "out"))
             )
         (call out> (fmt "[%s]\n" (format-time-string "%H:%M:%S")))

         ))





(defun >> (f &rest v)
  (let*()
    (with-buf "out"
      (goto (pt-max))
      (ins (apply 'fmt f v))
      )
    ))



(defun buf-append (buf f &rest v)
  (with-buf buf
    (goto (pt-max))
    (insert (apply 'fmt f v))))
(defun buf-prepend (buf f &rest v)
  (with-buf buf
    (goto (pt-min))
    (insert (apply 'fmt f v))))
(defun buf-clear/insert (buf f &rest v)
  (with-buf buf
    (del-region  (pt-min) (pt-max))
    (goto (pt-max))
    (insert (apply 'fmt f v))))



(defun make-f (buf)
  (prin1 (fmt "@@@@ [%s]\n" buf))
  (let*((f         (lambda ()
                     ;;(prin1 (fmt "[hi %s]\n" (format-time-string "%H:%M:%S")))
                     ;;(buf-prepend (format-time-string "%H:%M:%S"))
                     ;;(with-buf "out"
                     (with-buf buf
                       (del-region  (pt-min) (pt-max))
                       (goto (pt-max))
                       (insert (format-time-string "%H:%M:%S"))
                       )
                     )))
    f
  ))



(defun x () (interactive) ;;(/c)
       (let*((v    "out")
             ;;(f    (lambda () (prin1 (fmt "[hi %s]\n" v))))
             (f    (lambda ()
                     ;;(prin1 (fmt "[hi %s]\n" (format-time-string "%H:%M:%S")))
                     ;;(buf-prepend (format-time-string "%H:%M:%S"))
                     (with-buf v
                       (del-region  (pt-min) (pt-max))
                       (goto (pt-max))
                       (insert (format-time-string "%H:%M:%S"))
                       )
                     ))
             (f2     (make-f "out"))
             )
    ;;(prin1 (fmt "[%s]\n" f))
    (call f2)
    ;;(prin1 (fmt "[%s]\n" (get-named-frame "out")))
    ))


(defun x () (interactive) ;;(/c)
       (let*((fr              (open-bare-frame "out" ))
             (pars            (frame-parameters fr))
             >>)
         (! pars          `(,@pars  (out-buf . "out")))
         (! pars          `(,@pars  (out> .
                                          ;;(lambda () (prin1 (fmt "[hidare]\n" )))
                                          (lambda (f &rest v)
                                            (apply 'buf-append "out" f v)
                                            )
                                          )))
         (! >>         (cdr (aget 'out> pars)))
         (call >> "%s\n" (format-time-string "%H:%M:%S"))
         
         
         ;;(modify-frame-parameters fr pars)
         ;;(prin1 (fmt "r [%s]\n" fr))
         ;;(prin1 (fmt "r: [%s]\n" (aget 'test (frame-parameters fr))))
         ;;(prin1 (fmt "r: [%s]\n" (frame-parameters fr)))
         ;;(dolist (e  pars)   (prin1 (fmt "[%s]\n" e)))
         ))


(defun x () (interactive) ;;(/c)
       (let*((fr              (open-bare-frame "out" ))
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


