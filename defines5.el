
;;/  :system           (2)
;;|      buf-file-name
;;|      dir-path->subdir-name
;;|      file-path->subdir-name
;;|      make-file-rec
;;|      open-file-recs
;;|      make-file-rec2
;;|      open-file-recs2
;;|      backup-file?
;;|      dot-file?
(defun buf-file-name (&optional buf)
  (let*((path    (buf-file-path buf)))
    (if path
        (file-path->file-name path)
      nil)))
(defun dir-path->subdir-name (dir-path)  ;;/ problem
  ;;|  like:  "/aa/bb/cc/" -> "cc"
  (let*( last-ch last-i )
    (! last-i      (- (len dir-path) 1))
    (! last-ch     (substr dir-path last-i))    
    (if (str=? last-ch *path-delimiter*)
        (file-name-nondirectory  
         (substr dir-path 0 last-i))
      nil)))
(defun file-path->subdir-name (file-path)
   ;;|  like:  "/aa/bb/cc" -> "bb"
   (dir-path->subdir-name  (file-name-directory file-path)))

(defun make-file-rec (v &optional open?)  ;;/ uses older tlist struct
  (cond
   ((str? v)     (if (file-exists? v)
                     (let*((b     (path->buf v)))  ;; nil if not open
                       (when (and  (not b)  open?)  (find-file-noselect v))
                       `((:path . ,v) 
                         (:name . ,(path->file-name v))
                         (:buf  . ,b)))
                   nil))
   ((buffer? v)    (let*((p    (buf->path v)))
                     (if p
                         `((:path .  ,p)
                           (:name .  ,(path->file-name p))
                           (:buf  .  ,v))
                       nil)))
   (t            (error (fmt ":d - not a file or buffer [%S]" v)))))
(defun open-file-recs ()  ;;/ uses older tlist struct
    (filt-map   'make-file-rec   (buffer-list)))
(defun make-file-rec2 (v &optional open?)
  (cond
   ((str? v)     (if (file-exists? v)
                     (let*((b     (path->buf v)))  ;; nil if not open
                       (when (and  (not b)  open?)  (find-file-noselect v))
                       `(:path ,v  :name ,(path->file-name v) :buf ,b))
                   nil))
   ((buffer? v)    (let*((p   (buf->path v)))
                     (if p
                         `(:path ,p :name ,(path->file-name p) :buf ,v)
                       nil)))
   (t            (error (fmt ":d - not a file or buffer [%S]" v)))))
(defun open-file-recs2 ()
    (filt-map   'make-file-rec2   (buffer-list)))
(defun path->buf (path)
  (let*((path-match?  (ƛ (e) (let*((p  (buf-file-path e)))
                                (str=? path p))))
        (buf-lst      (filt  path-match? (buf-list))))
    (cond
     ((= (len buf-lst) 1)   (car buf-lst))
     ((> (len buf-lst) 1)   (error (fmt ":d - wtf")))
     (t                     nil))))
(defun backup-file? (s)     (= ?~  (aref  s  (- (len s) 1))))
(defun dot-file? (s)        (or (str=? "." s) (str=? ".." s)))
(defun trailing-path-delimiter? (s)
    (str=?  *path-delimiter*  (substr  s -1 nil)))

;;/ :dm - new -- from 011 -- func jump logic
(defun path->open-buf (path)
    (let*((path-match?/buf    (ƛ (r) 
                                 (if  (str=?  path  (tget. ':path r))
                                  (tget. ':buf r)   nil)))
          (lst                (filt-map   path-match?/buf   (open-file-recs))))
      (cond 
       ((> (len lst) 1)  (error (fmt ":d - ?")))
       ((null lst)       nil)
       (t                (car lst)))))

(defun path->open-file-rec (path)   ;;/ orig -- alist based
  (let*((open-lst    (open-file-recs))
        (want?       (ƛ (r) (str=?  (tget. :path r)  path))))
    (findf want? open-lst)))

;;/ :dm -- VERY-USEFUL
(defun path->open-file-rec2 (path &optional open-recs)
  ;;/ :dm note: '2' => OPEN-RECS must be plist -- generated by open-file-recs2
  (when (not open-recs)  (! open-recs   (open-file-recs2)))
  (let*((want?       (ƛ (r) (str=?  (pget  r ':path)  path))))
    (findf want? open-recs)))
(defun name->open-file-rec2 (name &optional open-recs)
  ;;/ :dm note: '2' => OPEN-RECS must be plist -- generated by open-file-recs2
  (when (not open-recs)  (! open-recs   (open-file-recs2)))
  (let*((want?       (ƛ (r) (str=?  (pget  r ':name)  name))))
    (findf want? open-recs)))


(defun d-ins-time-stamp () (interactive)
   (insert  (format-time-string 
             "%H:%M:%S"                   ;; use %k for hours for non 0 padded results
             ;;"%Y-%m-%d_%a %k:%M:%S"
             )))
;; (local-set-key   "\M-t"  'd-ins-time-stamp)  ;;| copy to buf as needed

(provide 'defines5)
