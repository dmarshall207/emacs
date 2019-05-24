
(defun setup-bare-frame (frame-name buf-name &optional specs noerror?)
  ;; :dm this might be useful in .emacs
  ;; :dm this was copied from plate: elisp/basic-3
  (let*((buf       (get-buf buf-name)))
    (when (not buf) (error (fmt ":d SETUP-OUTPUT-FRAME -- buf OUTPUT does not exists")))
    (let*((ori-fr   (cur-frame))
          (fr       (make-bare-frame frame-name specs t)))
      (cond
       (fr           (select-frame fr)
                     (switch-to-buf buf)
                     (select-frame ori-fr)
                     (raise-frame ori-fr))
       (noerror?     nil)
       (t            (error (fmt ":d SETUP-OUTPUT-FRAME -- frame already exists exists"))))
      fr)))

(provide 'my-frame-tools)
