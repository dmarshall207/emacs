;;/  :window     (2)
(defun split-win-vert (&optional win size)  ;;/ good
  (if (< size 0)
      (split-win win   (-  (win-height win)  (* -1 size)))
    (split-win win size)))
(defun split-win-horz (&optional win size)  ;;/ good
    (if (< size 0)
        (split-win  win   (-  (win-width win)  (* -1 size))   t)
      (split-win win size t)))
(defun split-win* (&optional win size horz)
    (when (< size 0)
        (! size   (if horz
                      (-  (win-width win)  (* -1 size))
                    (-  (win-height win)  (* -1 size)))))
    (split-win win size horz))

;;/  :buffer     (2)
;; :dm READ-ONLY-OFF/ON is my mistake in creating a misnomer
(defsubst read-only-off ()  (! inhibit-read-only t))
(defsubst read-only-on  () (! inhibit-read-only nil))
(defun /read-only-off () (interactive) (! inhibit-read-only t))
(defun /read-only-on  () (interactive) (! inhibit-read-only nil))

(defun erase-buffer* (&optional buf)
  ;;/ :dm ? should I check narrow state
  (when (not buf)  (! buf  (cur-buf)))
  (with-buf buf
    (let*((orig      inhibit-read-only))
      (when (not inhibit-read-only)
        (! inhibit-read-only  t))
      (erase-buffer)
      (! inhibit-read-only  orig))))
(defalias 'erase-buf*             'erase-buffer*)
(defun buf-narrowed? (&optional buf)
  (when (not buf)  (! buf  (cur-buf)))
  (with-buf buf
       (or  (not    (=  1 (pt-min)))
            (not    (=  (1+ (buffer-size)) (pt-max))))))


;;/  :window + :buffer   (2)
;;(defun setup-win/buf (win buf &optional createbuf noerr)        ;;/ :dm  KEY KEY EKY
;;  (let*((w     (selected-win)))
;;    (select-win/buf win buf createbuf noerr)
;;    (select-win w)))
(defun setup-win/buf (win buf &optional createbuf noerr)
  ;; Like SELECT-WIN/BUF but leaves the orig window selected.
  (let*((w    (selected-win)))
    (cond
        (win
              (cond               
                ((and (not (get-buf buf))  (not createbuf))
                      (if noerr  nil   (error (fmt ":d - SETUP-WIN/BUF - no such buf"))))
                (t
                      (select-win/buf win buf createbuf noerr)
                      (select-win w))))
        (t 
               (if noerr  
                   nil   (error (fmt ":d - SETUP-WIN/BUF - no such win")))))))
(defun select-win/buf (win buf &optional createbuf noerr)
  ;; Like SETUP-WIN/BUF but selects WIN as the selected window.
  (cond
    ((not (window? win))
             (if noerr   nil
               (error (fmt ":d - not a window [%S]" win))))
    ((and  (not (get-buf buf))  (not createbuf))
             (if noerr   nil
               (error (fmt ":d - not a buffer [%S]" buf))))
    (t
            (select-win win)
            (when (not  (str=?  buf (buf-name)))
              (switch-2-buf buf)))))

;;/  :frame          (tools)
;; convention:  all frames I create will have names and will be unique
(defun sel-frame (frame &optional input-focus?)
  (let*((fr    (if (str? frame) (frame/by-name frame) frame)))
    (cond
     (input-focus?    (select-frame-set-input-focus fr))
     (t               (select-frame fr)))))
(defun set-frame-buf (fr buf &optional inits)
  (let*((fr0    (cur-frame)))
    (sel-frame fr)
    (switch-2-buf buf)
    (mapc  (ƛ(fu)(⊙ fu))  inits)
    (sel-frame fr0)))

;;<sec>
;;/ info: 
;; Frame configuration recs are a bit confusing.
;; This is because there are numerious nested specs.
;; Also the car of the return list is the ‶superfluous″ 
;; frame-configuration which must be skiped over.
;; Thereafter each elm is a record for it's inner value/rec's:
;;     * the frame object
;;     * frame parameter alist
;;     * window-config object
;;/ howto iterate frame config recs
;;(defun x () (interactive)
;;  (let*(;;(lst              (cdr (current-frame-configuration))) ;;/ ALL frames
;;        (frams-recs         (cdr (current-frame-configuration))))
;;    (let*(fr-specs)  
;;      (while (! fr-specs  (pop frams-recs))
;;        (let*((fr-obj            (nth 0 fr-specs))
;;              (fr-parms          (nth 1 fr-specs))
;;              (fr-win-cfg        (nth 2 fr-specs)))
;;          (>>> (fmt "[%S] [%S]\n" 
;;                    (cdr (assq 'name fr-parms))
;;                    fr-obj)))))))
;;/ OR use ...
;;    * FRAME-LIST
;;    * FRAME-PARAMETER  (aka FRAME-PARM)
;;(defun x () (interactive)
;;  (let*((lst     (frame-list))
;;        fr)
;;    (while (! fr (pop lst))
;;      (>>> (fmt "[%S] [%S %S]\n" 
;;        (frame-parm fr 'name)
;;        (frame-parm fr 'top)
;;        (frame-parm fr 'left))))))
;;/ OR for a single/current frame
;;    (dolist (e  (frame-parameters (cur-frame)))
;;      (p (fmt "%S\n" e)))
;;</sec>

(defalias 'cur-frame    'selected-frame)

(defun frame-selected-buf (&optional frame)
  (let*((win     (frame-selected-win frame)))
    (if win  
        (win-buf win)
      nil)))
(defun frame-open? (name)
  (> (len (filter  (ƛ (f)  (str=? name (frame-parm f 'name)))
                   (frame-list))) 0))
(defun get-named-frame (name)
  (let*((target?  (ƛ (f)  (str=? name (frame-parm f 'name))))
        (lst      (filter  target?  (frame-list))))
    (if lst
        (if (= 1 (len lst))
            (car lst)
          (error (fmt ":d - oops - multiple frames have the same name [%S]" name)))
      nil)))
;;(defalias 'get-frame/by-named  'get-named-frame)
(defalias 'get-frame/by-name    'get-named-frame)
(defalias 'frame/by-name        'get-named-frame)

(defvar *bare-frame-defaults*
  `((menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (top . 80)     (left . 10)     ;; pixels
    (width . 90)   (height . 20)   ;; chars
    (border-width . 0)
    (internal-border-width . 0)
    (minibuffer . nil)
    (unsplittable . t)))
(defun open-bare-frame (name &optional buf specs nocreatebuf?)
  ;; :dm - should be named:  'open/create-bare-frame'
  (let*((get-frame  (ƛ () 
              (let*((target?  (ƛ (f)  (str=? name (frame-parm f 'name)))))
                (car  (filter  target?  (frame-list)))))))
    (cond 
     ((frame-open? name)
          ;;(p (fmt ":d - frame [%s] already open" name))
          (⊙ get-frame))
     (t   (let*((orig-frame   (selected-frame))
                (buf*         (if buf buf name)))
            (when (and nocreatebuf? (not (get-buf buf*)))
              (error (fmt ":d - oops - no such buffer [%S]" buf*)))
            (let*((fr       (make-frame  
                              `((name . ,name)  ,@specs  ,@*bare-frame-defaults*))))
              (select-frame-set-input-focus fr)
              (switch-2-buf   (get-buf-create buf*))
              (goto (point-max))
              (select-frame-set-input-focus  orig-frame)
              ;;(raise-frame orig-frame)
              fr))))))
(defun make-bare-frame (name &optional specs noerror?)   ;;/ :new
  (let*((fr    (get-named-frame name)))
    (cond
     ((and fr noerror?)
                nil)
     (fr        (error (fmt ":d - ERR: frame [%S] already exists" name)))
     (t         (let*((ori-frame   (cur-frame))
                      (fr          (make-frame 
                                      `((name . ,name)  ,@specs  ,@*bare-frame-defaults*) )))
                  ;;(select-frame ori-frame)
                  (select-frame-set-input-focus  ori-frame)
                  fr)))))

(defvar *msg-frame* nil)
(defun msg-frame? (v)
  (and (framep  v)
       (str=? "*msg-frame*" (frame-parameter v 'name))))
(defun msg-frame-open? ()
    (> (len (filter 'msg-frame? (frame-list))) 0))
(defun /open-msg-frame () 
  (interactive)
  (when (msg-frame-open?)
    (error (fmt ":d - *msg-frame* already open")))
  (let*((orig-frame    (selected-frame)))    
    (! *msg-frame* (open-bare-frame "*msg-frame*"))
    (select-frame-set-input-focus *msg-frame*)
    (switch-2-buf "*Messages*")
    (goto (point-max))
    (select-frame-set-input-focus  orig-frame)))

(defun d-set-frame-font (size-spec &optional fr)
  ;; as a command or a function
  (interactive (list
      (compl-read/1 "input ↓ : "  '("default" "small"))))
  (let*((initial-frame   (cur-frame)))
    (cond
      ((null fr)       (! fr        (cur-frame)))
      ((str? fr)       (! fr        (get-named-frame fr))))
    (let*((my-default "-unknown-DejaVu Sans Mono-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1")
          (small      "-unknown-DejaVu Sans Mono-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1")
          ;;(a          "-unknown-DejaVu Sans Mono-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1")

          (the-font   (cond
                        ((str=? "default" size-spec)    my-default)
                        ((str=? "small"  size-spec)     small)
                        (t        (error (fmt "oops - invalid selection"))))))
      (select-frame fr)
      (set-frame-font the-font  t)
      (select-frame initial-frame))))
;;(defun d-set-frame-font (v)  (interactive (list
;;      (compl-read/1 "input ↓ : "  '("default" "small"))))
;;  ;;/ :dm  Linux tools tools for finding fonts:
;;  ;;         * xlsfonts
;;  ;;         * xfontsel
;;  (let*((my-default "-unknown-DejaVu Sans Mono-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1")
;;        (small      "-unknown-DejaVu Sans Mono-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1")
;;        selected-font)
;;    (! selected-font   (cond
;;                        ((str=? "default" v)    my-default)
;;                        ((str=? "small" v)      small)
;;                        (t                      (error (fmt "oops - invalid selection")))))
;;    (set-frame-font selected-font  t)))


;; ==================================================================
;; 05/17/19 -- new -- new install 'vmubu2'  ("vitual machine ubuntu 2")






(provide 'defines3)
