
;;/ This file:   (section-1)
;;|   last-copy-to-buf
;;|   month/name↗digit-str
;;|   load-my-abbrevs
;;|   day/time-stamp-str/fmt-1
;;|   d-copy-region-to-buf
;;|   d-remove-following-blank-lines
;;|   str-ends-w-newline-p
;;|   remove-ending-newline
;;|   current-line-txt
;;|   file-spec.name
;;|   file-spec.type
;;|   file-spec.hard-links-count
;;|   file-spec.uid
;;|   file-spec.gid
;;|   file-spec.last-access
;;|   file-spec.last-mod
;;|   file-spec.last-status-change
;;|   file-spec.size
;;|   file-spec.modes
;;|   file-spec.gid-would-change
;;|   file-spec.inode
;;|   file-spec.file-sys-num
;;|   insert-at-current-column
;;|   read-from-options-list
;;|   make-completing-read-collection
;;|   make-dir-tree-itr
;;|   dir-listing        ;; :dlm -- use 'directory-files-and-attributes' instead
;;|   file-attribs-plist
;;|   file-accessibility-plist
;;|   save-clip-to-buf
;;|   xor
;;|   *gensym-counter*
;;|   gensym
;;|   multi-set
;;|   open-buf-win
;;|   one-win-only
;;|   set-buf-local-vars
;;|   line-number-at-pos
;;|   set-buf-local-map
;;|   ls-files
;;|   ls-dirs
;;|   new-arec
;;|   norm-path
;;|   str-join
;;|   setnth
;;|   delnth
;;|   in-listp
;;|   add-to-load-paths
;;|   db
;;|   db2
;;|   d-color
;;|   findf
;;|   memf
;;|   mapcar*
;;|   bol-str=?
;;|   d-toggle-paren/brack
;;|   int->str/base
;;|   path->file
;;|   path->dir
;;|   parent-dir?
;;|   flash-range
;;|   flash-region
;;|   thing-at-point*
;;|   cur-list-range

;;/ This file:   (section-2)
;;|    file-name-base
;;|    file-name-ext                  (moved to .emacs)
;;|    file-path->file-name
;;|    file-path->dir-path
;;|    buf-file-path
;;|    buf-file-name
;;|    dir-path->subdir-name
;;|    file-path->subdir-name
;;|    buffer-list->open-file-recs

;;/ This file:   (section-3)
;;|    *mri-re*
;;|    *dir-mark-re*
;;|    make-dir-mark
;;|    get-dir-dir-mark

;;/ This file:   (section-4)
;;|    d-backup-log-report
;;|    random-ri
;;|    collect-functions
;;/ :target


;;/==================================================================
;;/==================================================================
;;/ <SECTION-1>
;<var>
(defvar last-copy-to-buf "")
;</var>

;<var>
(defvar month/name↗digit-str
      (make-hash-table :test 'equal))
;</var>

;<load-hash>
(let((dat '("jan" "01"  "feb" "02"  "mar" "03"  "apr" "04"  "may" "05" 
            "jun" "06"  "jul" "07"  "aug" "08"  "sep" "09"  "oct" "10" 
            "nov" "11"  "dec" "12")))
  (while (not (null dat))
    (let*((month   )
          (day     ))
      (apply  'puthash    (list  (nth 0 dat)  (nth 1 dat)  month/name↗digit-str))
      (setq  dat  (nthcdr 2 dat)))))
;</load-hash>


;<fu>
(defun load-my-abbrevs (alst)
  (mapc (lambda (p)
          (define-abbrev  local-abbrev-table  (car p)  (cdr p)) )
        alst))
;;<usage>
;;    (load-my-abbrevs
;;     '(("s"     . "∑")  
;;       ("p"     . "∫")
;;       ("r"     . "∏-ref") 
;;       ("s"     . "∏-set!") 
;;       ("d"     . "ⵠ")))
;;</usage>
;</fu>


;<fu>
(defun day/time-stamp-str/fmt-1 ()
  (let*((v   (current-time-string)))      ;; like: "Tue Nov 15 18:08:45 2011"
    (fmt "%s-%s-%s %s"
            (substr v 20)                 ;; year
            (downcase (substr v 4 7))     ;; month
            (substr v 8 10)               ;; day
            (substr v 11 19))))           ;; time
;</fu>


;<fu>
(defun d-copy-region-to-buf (&rest args)
  (interactive)
  (if (not mark-active)    (error "err: mark not active"))
  (let* ((edit-buf      (current-buffer))
         (txt          (buf-substr (r-start) (r-end)))
         ;(txt           "some text for testing")
         (dst-buf       (if (> (length args) 0) (car args) nil))
         (validat-buf   (lambda (s)
                           (if (not (member (get-buffer s) (buffer-list)))
                               (error "err: not a valid buffer"))))
         (prompt-buf    (lambda ()
                           (read-string "to buffer: " last-copy-to-buf nil ""))))
    (if (and dst-buf
             (not (member  (get-buffer dst-buf)  (buffer-list))))
        (error "err: 'copy-region-to-buf' was passed an invalid buffer"))
    (when (not dst-buf)
        (setq dst-buf            (call prompt-buf))
        (call validat-buf        dst-buf)
        (setq last-copy-to-buf   dst-buf))
    (set-buffer   dst-buf)
    (insert       txt)
    (set-buffer   edit-buf)))
;</fu>

;;<fu>
;;<usage>
;; ;; How to use it and automatically move forward
;;    (defun x ()
;;      (interactive)
;;        (d-remove-following-blank-lines)
;;        (srch-re-> "^[ \t]*$")
;;        (forward-line -1))
;;</usage>
(defun d-remove-following-blank-lines ()
  "Removes the blank lines from the end of current line up to
   the next non-blank line."
  (interactive)
  (let* ((start)(end))
    (eol)
    (setq start (point))
    (srch-re-> "[^ \n\t]")
    (bol)
    (goto  (- (point) 1))
    (del-region start (point))))
;;</fu>


;<fu>
(defmacro str-ends-w-newline-p (s)
  "predicate macro: Returns true if the last char in a string
  is a newline char. (note: can be intermediate
   newline chars)"
 `(= 10 (elt ,s (1- (length ,s)))))
;</fu>

;<fu>
(defmacro remove-ending-newline (s)
  "macro;  Returns a new string that is insured to not have 
   a newline char as the last char. (note: can be intermediate
   newline chars)"
    `(if (str-ends-w-newline-p ,s)
        (substring ,s 0 (1-(length ,s)) )
      ,s))
;</fu>

;<fu>
(defmacro current-line-txt ()
  "macro: returns the text on the current line (w/o a newline
  char."
  '(buf-substr (bol-pos) (eol-pos)))
;</fu>

;<fu>
;</fu>


;;<group>
;- File Attributes Description  (file-spec.ATTR-NAME)
;|  name.png      :   0  : file name
;|  nil           :   1  : dir/symlink/txt
;|  1             :   2  : hard links count
;|  1000          :   3  : uid
;|  1000          :   4  : gid
;|  (19164 58286) :   5  : time of last access
;|  (18316 57134) :   6  : time of last modification
;|  (18477 65391) :   7  : time of last status change
;|  93693         :   8  : size of the file in bytes
;|  -rw-r--r--    :   9  : file's modes
;|  nil           :  10  : `t' => GID would change if deleted and recreated
;|  393218        :  11  : inode number
;|  842           :  12  : file system number
;|
;| Macros that operate on the list of values returned
;| by 'file-attributes'
(defmacro file-spec.name (spec)                  `(elt ,spec 0))
(defmacro file-spec.type (spec)
  `(let*((v   (elt ,spec 1)))
     (cond
       ((null v)                    'reg)
       ((eq 'string (type-of v))    'lnk)
       (t                           'dir))))
(defmacro file-spec.hard-links-count (spec)
  `(elt ,spec 2))
(defmacro file-spec.uid (spec)
  `(elt ,spec 3))
(defmacro file-spec.gid (spec)
  `(elt ,spec 4))
(defmacro file-spec.last-access (spec &optional as-str)
  `(if ,as-str
       (current-time-string  (elt ,spec 5))
     (elt ,spec 5)))
(defmacro file-spec.last-mod (spec)
  `(if ,as-str
       (current-time-string  (elt ,spec 5))
     (elt ,spec 6)))
(defmacro file-spec.last-status-change (spec)
  `(if ,as-str
       (current-time-string  (elt ,spec 5))
     (elt ,spec 7)))
(defmacro file-spec.size (spec)
  `(elt ,spec 8))
(defmacro file-spec.modes (spec)
  `(elt ,spec 9))
(defmacro file-spec.gid-would-change (spec)
  `(elt ,spec 10))
(defmacro file-spec.inode (spec)
  `(elt ,spec 11))
(defmacro file-spec.file-sys-num (spec)
  `(elt ,spec 12))
;;</group>


;;<fu>
(defun insert-at-current-column (txt)
  (let* ((col     (current-column))
         (start   (point))
         (margin  (make-string col ? ))
         end-marker)
    (save-excursion
        (insert  txt)
        (setq end-marker  (point-marker)))
    (goto  start)
    (forward-line)
    (dotimes  (i  (1- (count-lines start end-marker)))
        (insert margin)
        (forward-line))
    (goto   end-marker)
    (set-marker  end-marker  nil)  ;| ?? garbage collected ??
)) 
;;</fu>




;;<macro>
;;<usage>
;;    (setq entry-list (list "one_one" "one_two" "one_three"))
;;    (setq s  (read-from-options-list "item : " entry-list) )
;;</usage>
;------------------------------------------------------------------
(defmacro read-from-options-list (prompt str-list &optional require-match)
     `(completing-read
            ,prompt
            (make-completing-read-collection ,str-list)
            nil                  ; predicate
            ,require-match       ; domain: nil, t, other
            nil                  ; initial
            ',str-list           ; history
            )
) 
;;</macro>



;;<fu>
;;<usage>
;;    (setq entry-list (list "one_one" "one_two" "one_three"))
;;    (setq s
;;          (completing-read
;;                "input : "
;;                (make-completing-read-collection entry-list)
;;                nil                  ; predicate
;;                nil                  ; require-match
;;                nil                  ; initial
;;                'entry-list          ; history
;;                nil                  ; default
;;                )
;;     )
;;</usage>
;-----------------------------------------------------------------
(defun make-completing-read-collection(entry-list)
  (let ((i 0)  
        collection)
    (mapc
       (lambda (r)
          (setq collection 
                (nconc collection
                       (list (cons r 
                                   (setq i (1+ i)))))))
       entry-list)
    collection
))
;;</fu>

;;---------------------<<< above ok

;;<fu>
;------------------------------------------------------------------
(defun make-dir-tree-itr (root-dir)
"Returns an iterator function that returns all directories of
a tree, one for each call."
  (let* ((hold         (gensym)))
    (set hold   (list (list root-dir)))
    `(lambda ()
       (let* ((lst     (pop ,hold))
              (d       (pop lst)))
         (cond 
             (d
                (if lst     (push lst ,hold))
                (setq lst   (reverse (pget (dir-listing d) :dirs)))
                (if lst     (push lst ,hold))))
         d))
))
;;</fu>


;;<fu>
;; :dlm -- use 'directory-files-and-attributes' instead
;------------------------------------------------------------------
(defun dir-listing (dir-path &optional full-name)
  (let* ((dirs) (files) (links) (special))
    (dolist (path (directory-files dir-path t "[^~]$"))
      (let*((name        (if full-name
                             path
                           (car (last (split-string  path "/"))))))
        (cond
            ((or  (equal "." name)  (equal ".." name))   nil)
            ((file-directory-p path)   (push name  dirs))
            ((file-symlink-p   path)   (push name  links))
            ((file-regular-p   path)   (push name  files))
            (t                         (push name  special)))
      ))
    `(:dirs ,dirs :files ,files :links ,links :special ,special)
))
;;</fu>

;;<fu>
;------------------------------------------------------------------
(defun file-attribs-plist (path)
  "Returns a plist of file attribute key/values for the file/dir 
specified in PATH."
  (let* ((raw      (file-attributes  path))
         (val      (pop raw))
         atrs)
    (cond
        ((eq  (type-of val) 'string)
                 (pput* atrs   :sym-link val)
                 (if (file-directory-p  (file-truename path))
                        (pput* atrs   :is-dir  t)
                     (pput* atrs   :is-dir  nil)))
        (t
           (pput* atrs   :sym-link  nil)
           (pput* atrs   :is-dir    val)))
        
    (dolist  (key  (list :hard-links  :uid  :gid  
                         :atime  :mtime  :ctime  :size :modes 
                         :gid-chg  :inode  :filesys-no ))
         (pput* atrs key  (pop raw)))
    atrs
)) 
;;</fu>


;;<fu>
;------------------------------------------------------------------
(defun file-accessibility-plist (path)
  (let* ((info))
    (if (not (file-exists-p path))
             (setq info     (list :exists nil))
      (setq info     (list :exists t))
      (pput* info  :readable   (file-readable-p   path))
      (pput* info  :writable   (file-writable-p   path))
      (pput* info  :executable (file-executable-p path)))
    info
)) 
;;</fu>



;;<fu>
;------------------------------------------------------------------
(defun save-clip-to-buf (&rest karg)
  "Copies a \"clip\" from current buf to either a specified buf or
the *scratch* buf if one is not specified.
If start/end args are specified they define the clip/region.
If start/end args are not specified the clip text is defined by
mark and point if mark is active or the current line if mark is
inactive."
  ;;(interactive)
  (let* ((src-buf     (current-buffer))
         (start       (pget  karg  :start))
         (end         (pget  karg  :end))
         (to-buf      (or    (pget  karg  :to-buf)  
                             "*scratch*"))
         (after       (pget  karg  :after))
         (before      (pget  karg  :before))
         (regex       (or    before after)))
    
    (if (and before after)
        (error "err: save-clip-to-buf: args before/after are mutually exclusive"))
    (if (xor  start end)
        (error "err: save-clip-to-buf: args start/end must both be un/specified"))
    (if (or  (not start) (not end))
        (cond
           (mark-active  ;_ selected region defines start/end
                 (setq  start  (min  (mark) (point)))
                 (setq  end    (max  (mark) (point))))
           (t            ;_ current line defines start/end
                 (setq  start  (bol-pos))
                 (setq  end    (eol-pos)))))
    (with-current-buffer to-buf
      (save-excursion
        (cond
           ((or  before after)
                  (goto    (pt-min))
                  (if (not (srch-re->  regex nil t))
                      (error "err: save-clip-to-buf: karg :after not found"))
                  (if before   (goto   (m-start 0))))
           (t
                  (goto    (pt-max))))
        (cond  ;_ if cursor not at eol insert newline after current line
           ((/= 0 (current-column))
                (eol)
                (ins "\n")))
        ;_ insert clip
        (insert-buffer-substring   src-buf start end)
        (ins "\n")))
)) 
;;</fu>


;;<mac>
(defmacro xor (a b)
  `(cond 
      ((and  ,a ,b)
           nil)
      ((and  (eq  (not (not ,a))  nil)
             (eq  (not (not ,b))  nil))
           nil)
      (t
           t))
)
;</mac>

;;-----------------------------<<<<< above ok




;;<group>
;; These were copied from the `cl.el' file
;;<var>
;;(defvar *gensym-counter*)
(defvar *gensym-counter* 100)
;;</var>
(defun gensym (&optional arg)
  "Generate a new uninterned symbol.
The name is made by appending a number to PREFIX, default \"G\"."
  (let ((prefix (if (stringp arg) arg "G"))
        (num (if (integerp arg) arg
               (prog1 *gensym-counter*
                 (setq *gensym-counter* (1+ *gensym-counter*))))))
    (make-symbol (format "%s%d" prefix num))))
;;</group>





;;<fu>
;;<usage>
;;      (let* (one two three)
;;          (multi-set (list 'one 'two 'three) (list 11 22 33))
;;          (p (fmt "[%s]" one )))
;;</usage>
;------------------------------------------------------------------
(defun multi-set (syms vals)
    "Sets list of syms (symbols/vars) to the corresponding vals values"

    ;; ??? 
    ;;(if (not (= (length syms) (length vals)))
    ;;    (error "err: multi-set args must be same length"))
    
    (while syms
      (let* ((sym    (pop syms))
             (val    (pop vals)))
        (if (and   (not  (eq sym nil))
                   (eq   (type-of sym) 'symbol))
            (set sym val)) ))
) 
;;</fu>



;;<fu>
;------------------------------------------------------------------
(defun open-buf-win (&rest karg)
  ;; NOT for opening a file -- rather a buf NOT assoc w/ a file
  "Combines the operations of opening a window and a buffer into
   one command."
  (let* 
      (
       (at        (pget  karg :at))
       (buf       (pget  karg :buf))
       (buf       (cond ((eq   nil  buf)
                                      (get-buffer-create  (make-temp-name "GEN")))
                        ((eq   'string  (type-of buf))
                                      (get-buffer-create   buf))
                        ((eq   'buffer  (type-of buf))
                                       buf) ))
       (vert        (pget  karg :vert))
       (clear       (pget  karg :clear))
       (win         (or    (pget  karg :win)  
                           (selected-window)))
       (new-win)
       ;;(new-win     (select-window   (split-window   nil at vert)))
       )

    (one-win-only)     ;; ???  <--- does not really belong 
    (setq new-win     (select-window   (split-window   nil at vert)))
    
    (switch-to-buffer  buf)
    (if clear          (del-region  (pt-min) (pt-max)))
    (select-window     win)
    ;;(list buf new-win)
    buf

    ;;(one-win-only)     ;; ???  <--- does not really belong 
    ;;(select-window     (split-window   nil at vert))
    ;;(switch-to-buffer  buf)
    ;;(if clear          (del-region  (pt-min) (pt-max)))
    ;;(select-window     win)
    ;;(list buf win)

)) 
;;</fu>




;;------------------------<<< above ok






;;<mac>
;------------------------------------------------------------------
(defmacro one-win-only ()
    '(if (not (one-window-p))  (delete-other-windows))
)
;;</mac>


;;; ???????????
;;;<fu>      <<<<<<<<<<<<<< :HERE :ERRR ????
;;           Error in menu-bar-update-hook: (wrong-type-argument stringp list-buffers-directory)
;;; ???????????
;;;------------------------------------------------------------------
;(defun buffer-local-value (var buf)
;    (with-current-buffer buf
;        var)
;) 
;;</fu>



;;<fu>
;;<usage>
;;    (set-buf-vars  some-buf 
;;                       '(one . 11) 
;;                       '(two . 22) )
;;</usage>

; :test
;------------------------------------------------------------------
(defun set-buf-local-vars (target-buf &rest name-val-pairs)

  (let* ((target-buf       (get-buffer target-buf)))

    (with-current-buffer target-buf    
          (dolist  (pair  name-val-pairs)
              (let*((sym     (car pair))
                    (val     (cdr pair)))
                (make-variable-buffer-local sym)
                (set sym  val)
            )))
))


;; :curr-ver
;;;------------------------------------------------------------------
;;(defun set-buf-local-vars (target-buf &rest name-val-pairs)
;;  (let* ((target-buf       (get-buffer target-buf)))
;;    (with-current-buffer target-buf    
;;          (dolist  (pair  name-val-pairs)
;;              (let*((sym     (car pair))
;;                    (val     (cdr pair)))
;;                (make-variable-buffer-local sym)
;;                (set sym  val)
;;            )))
;;))


;;</fu>


;;<fu>
;; found this in : http://www.emacswiki.org/cgi-bin/wiki/misc-cmds.el
;------------------------------------------------------------------
(unless (fboundp 'line-number-at-pos)   ; Exists in Emacs 22.
  (defun line-number-at-pos (&optional pos)
    "Buffer line number at position POS. Current line number if POS is nil.
     Counting starts at (point-min), so any narrowing restriction applies."
    (1+ (count-lines (point-min) (save-excursion (when pos (goto-char pos))
                                                 (forward-line 0) (point))))))
;;</fu>


;;<fu>

;------------------------------------------------------------------
(defun set-buf-local-map (kmap &optional buf)
  ;; :todo : add type checking  
    (if (not buf)    (setq buf  (current-buffer)))
    (with-current-buffer buf
          (use-local-map     kmap))
    kmap
)
;;</fu>


;;<fu>
(defun ls-files (path &optional full-name regex)
  (let ((file-list))
    (dolist (name (directory-files path full-name regex) )
      (if (not (file-directory-p name))
          (setq file-list   
                       (nconc   file-list (list name))))
      )
    file-list
))
;;</fu>

;;<fu>
(defun ls-dirs (path &optional full-name regex)
  (let ((dir-list))
    (dolist (name (directory-files path full-name regex) )
      (if (file-directory-p name)
          (setq dir-list   
                       (nconc   dir-list (list name))))
      )
    dir-list
))
;;</fu>


;;==================================================================
;; "record"

;------------------------------------------------------------------
(defun new-arec (proto &rest kargs)
  (let* ((rec   (copy-sequence proto)))    
    ;;(db (fmt "2) proto : %s" proto))
    ;;(db (fmt "2) kargs : %s" kargs))
    (while kargs
        (let* ((name   (pop kargs))
               (val    (pop kargs))
               (pair   (agetq name rec)))
          (if pair
              (setcdr     pair  val)
              (setq rec     (nconc  rec  (list `(,name . ,val))))            
            )
      ))
    rec
)) 
(put 'new-arec :def-file "dlm.el")

;; Usage examples :

;; ;; Usage #1 :    Direct call to `new-arec' w/ all information needed
;; ;;               for the values included in the alist
;; ;------------------------------------------------------------------
;; (defun usage-1 ()
;;   (interactive)
;;   (let* ((rec   (new-arec  '((:dept   . "Information Systems") 
;;                             (:phone  . "800-222-info")))))
;;     (p (fmt "results : %s"  rec))
;; ))
;; ;; Usage #2 :    Direct call to `new-arec' but with a prototype rec
;; ;;               that can have specific values changed by adding 
;; ;;               a labeled argument list.
;; ;------------------------------------------------------------------
;; (defun usage-2 ()
;;   (interactive)
;;   (let* ((proto     '((:dept  . nil) 
;;                       (:phone . "800-222-info"))))
;;     (db (fmt "results : %s"  
;;                           (new-arec proto :dept "Planning")))
;;     (db (fmt "results : %s"  
;;                           (new-arec proto :dept "Marketing")))
;; )) 
;; ;; Usage #3 :   Calling `new-arec' via an intermediary that defines
;; ;;              the prototype. Advantage is ability to define consistency
;; ;;              controls and logic to collect or generate values.
;; ;------------------------------------------------------------------
;; (defun usage-3 ()
;;   (interactive)
;;   (let* ()
;;     (db (fmt "results : %s"  
;;                      (new-dept-rec :dept "Quality Control")))
;;     (db (fmt "results : %s"  
;;                   (new-dept-rec :dept nil)))
;; ))
;; ;------------------------------------------------------------------
;; (defun new-dept-rec (&rest kargs)
;;   (let* ( (proto    '( (:dept  . nil)
;;                        (:phone . "800-222-info"))) )
;;     
;;     (if (not (pget :dept kargs))
;;              (error "new-dept-rec : must pass :dept argument value")
;;              )
;;     (eval   `(new-arec  proto ,@kargs))
;; ))


;; ------------------------------<<<< above ERR




;;==================================================================

;; :good -- untested
;------------------------------------------------------------------
(defun norm-path (path)
  (let* ()
    (cond 
       ((eq  'windows-nt system-type)
                           (replace-regexp-in-string 
                                         "/"  "\\\\\\\\"  path) )
       ((eq  'gnu/linux system-type)
                           (replace-regexp-in-string 
                                         "\\\\"  "/"  path) )
       )
))
(put 'norm-path :def-file "dlm.el")


;; :good --  untested
;------------------------------------------------------------------
(defun str-join (lst sep)
  " @lst   - list of strings
    @sep   - seperator str to use in joining
  "
  (let* ( (str    (car lst))
          (r      (cdr lst)) )
    (while r
      (setq str  (concat  str  sep (car r)))
      (setq r    (cdr r))
      )
    str
))
(put 'str-join :def-file "dlm.el")

;;<mac>
;------------------------------------------------------------------
(defmacro setnth (lst n val &optional noerr)
    (if noerr
          `(condition-case nil
               (setcar (nthcdr ,n lst) ,val)
             (error nil) )
        `(setcar (nthcdr ,n lst) ,val) )    
)
(put 'setnth :def-file "dlm.el")
;;</mac>

;;<mac>
;------------------------------------------------------------------
(defmacro delnth (n lst &optional noerr)  
  (if noerr      
      `(condition-case nil
           (let* ( (u  (nthcdr (1- ,n) ,lst))
                   (v  (nthcdr ,n      ,lst)) )
             (pop v)
             (setcdr u v) )
         (error nil) )

    `(progn
       (let* ( (u  (nthcdr (1- ,n) ,lst))
               (v  (nthcdr ,n      ,lst)) )
         (pop v)
         (setcdr u v) ))
))
(put 'delnth :def-file "dlm.el")
;;</mac>



;; return t if var in lst -- only tested for string elements
;------------------------------------------------------------------
(defun in-listp (val lst)
  "returns t if var in lst"
  (catch 'exit
            (dolist (v lst)
                (if (equal  v val)
                    (throw 'exit t) ))
      nil)
)
(put 'in-listp :def-file "dlm.el")

;; add list of paths to emacs' load-path
;; todo : check case sinsitivity
;------------------------------------------------------------------
(defun add-to-load-paths (drive-letter paths-list)
  (let* (new)
    (dolist (v  paths-list)
        (setq v   (fmt "%s\%s" drive-letter v))
        (if (not (in-listp v load-path))
            (setq new    (append new (list v)))))
    (if (> (length new) 0)   
        (setq load-path    (append new load-path)))
)) 
(put 'add-to-load-paths :def-file "dlm.el")



;------------------------------------------------------------------
(defun db (prompt)  

  (let* (  )
    (setq response  (read-from-minibuffer prompt))
    (if (equal "" response)
        nil
      response)
)) 
(put 'db :def-file "dlm.el")


;;;------------------------------------------------------------------
;;(defun db (prompt)  
;;  (let* ( (response  (read-from-minibuffer prompt)) ) 
;;    (if (equal "" response)
;;        nil
;;      response)
;;)) 
;;(put 'db :def-file "dlm.el")



;------------------------------------------------------------------
(defun db2 (text)  
  (let* ()
    (set-buf    "*scratch*")
    (delete-region    (pt-min) (pt-max))
    (insert text)
)) 
(put 'db2 :def-file "dlm.el")


;;/ exploring using color-theme
;;(require 'color-theme)
;;(require 'zenburn)
;;(require 'color-theme-dlm)
;;(defun d-color ()
;;  (interactive)
;;  (let*( (theme-lst       '("dlm" "zenburn")) 
;;         (selection       (read-from-options-list "color theme : " theme-lst) ))
;;    (cond
;;       ((str= "dlm" selection)       (color-theme-dlm))
;;       ((str= "zenburn" selection)   (color-theme-zenburn))
;;       (t     (p (fmt "no such color scheme in THEME-LST" ))))))


(defun findf (fu lst)
  (cond
   ((eq nil lst)           nil)
   ((call fu (car lst))    (car lst))
   (t                      (findf fu (cdr lst)))))

(defun memf (fu lst)
  (cond
   ((eq nil lst)           nil)
   ((call fu (car lst))    lst)
   (t                      (memf fu (cdr lst)))))


(defun mapcar* (function &rest args) ;;/ from elisp docs
  "Apply FUNCTION to successive cars of all ARGS.
Return the list of results."
  ;; If no list is exhausted,
  (if (not (memq nil args))
      ;; apply function to CARs.
      (cons (apply function (mapcar 'car args))
            (apply 'mapcar* function
                   ;; Recurse for rest of elements.
                   (mapcar 'cdr args)))))

;;/==================================================================




(defun bol-str=? (target)
    (str=  target  
           (buf-str-wo (bol-pos) (+ (len target) (bol-pos)))))

(defun d-toggle-paren/brack ()
  (interactive)
  (let*((start        (r-start))
        (end          (r-end))
        (start-ch     (buf-str-wo (r-start) (1+ (r-start)))) 
        (end-ch       (buf-str-wo (1- (r-end)) (r-end)))  
        (set-ch       (lambda (pos ch) 
                        (save-excursion
                          (goto pos)
                          (del-ch 1)
                          (insert ch)))))
    (when (not mark-active)
      (error "dlm/err: mark not active"))
    (cond
     ((and (str= "(" start-ch) (str= ")" end-ch))
            (call set-ch start      "[")
            (call set-ch (1- end)   "]")
            (goto start))
     ((and (str= "[" start-ch) (str= "]" end-ch))
            (call set-ch start     "(")
            (call set-ch (1- end)  ")")
            (goto start))
     (t
            (error "dm/err: region start/end are not parentheses/brackets")
     ))))

(defun int->str/base (n  base  &optional upper-case)
  (let*((letter-base   (if upper-case ?A ?a))
        (n->letter        (lambda (n)  (string   
                         (cond  ((< n 10)    (+ n ?0))
                                (t           (+ (- n 10) letter-base))))))
        (sto      '()))
    (while (>= n base)
      (let*((quo   (/    n  base))
            (rem   (mod  n  base)))
        (setq sto        (cons (call n->letter rem) sto))
        (setq n          quo)))
    (setq sto     (cons   (call n->letter n)   sto))
    (apply 'str+ sto)))

(defun path->file (path)
    (substr path  (string-match "[^/]+$" path)))

(defun path->dir (path)
    (substr path  0  (1- (string-match "[^/]+$" path))))

(defun parent-dir? (path-1 path-2)
    (str=  path-1  (path->dir path-2)))


(defun flash-range (rng &optional time)
  (flash-region (nth 0 rng) (nth 1 rng) time))

(defun flash-region (start end  &optional time)
  (when (not time)  (setq time 1))
  (font-lock-mode 0)
  (facemenu-set-background 
     "gray16"
     start end)
  (sit-for time)
  (font-lock-mode 1))

(defun thing-at-point* (type)
  (let*()
    (let*((v      (thing-at-point type)))
      (if v
          (substring-no-properties  v)
        nil))))


(defun cur-list-range ()
  (save-excursion
    (let*( end-pos )
      (! end-pos  (condition-case err 
                      (let*()
                        (up-list) 
                        (point))
                    (scan-error  
                     ;;(p (fmt ":d - Not inside a list"))  
                     nil)))
      (cond
       (end-pos      (backward-list)
                     `(,(point) . ,end-pos))   ;;| range 
       (t            nil)))))
  
;;| </SECTION-1>

;;/============================================================================
;;/============================================================================
;;/ <SECTION-2>
;;/      | System file path/name logic

;;/             :dm moved to .emacs  :system

;;| </SECTION-2>


;;/============================================================================
;;/============================================================================
;;/ <SECTION-3>


(defvar *mri-re*
  (let*((s  "[A-Za-z0-9]"))
    (str+ s s s s s s s s s)))

(defvar *dir-mark-re*
                 (let*((s  "[A-Za-z0-9]"))
                   (str+ "DIR@" s s s s s s s s s)))


(defun make-dir-mark (&optional mri)  ;;/ good
  (! mri    (if mri   mri   (shell-command-to-string "/sto/bin/gen-mri")))
  (let*((valid-mri-format?   (re-str-match  (str+ "^" *mri-re* "$")  mri)))
    (if valid-mri-format?
        (fmt "DIR@%s" mri)
      nil)))

(defun get-dir-dir-mark (dir-path)
  (if (not (dir-exists? dir-path))
      nil
    (let*((dir-mark-files   '())
          (re               (str+ "^DIR@" *mri-re* "$")))      
      (dolist (s  (dir-files dir-path))
        (when (re-str-match re s)     (push s dir-mark-files)))
      (cond
         ((null dir-mark-files)         nil)
         ((= 1 (len dir-mark-files))    (car dir-mark-files))
         (t                             (error ":d - multiple dir-mark files"))))))

;;| </SECTION-3>

;;/============================================================================
;;/============================================================================
;;/ <SECTION-4>
(defun d-backup-log-report ()  
  (interactive)
  (let*(;; ---------------------------------------------------------------------
        (backup-log?   (ƛ (r)
                (let*((p     (tget. ':path r))
                      (lst   (reverse (split-str p "\/" t))))
                  (and (> (len lst) 1)
                       (equal?  (take lst 2)  '("BACKUP-LOG" "BACKUP"))))))
        ;; ---------------------------------------------------------------------
        (do-report     (ƛ (p)                 
                (let*((report-entry  (ƛ (dat)
                           (let*((file-entry?     (ƛ (e)
                                                     (if (and (list? e) (eq? ':file (car e)))
                                                         (car (tget.':name e))
                                                       nil))))
                             (dolist (e  (filt-map file-entry? dat))
                               (⊙ out> (fmt "%s\n" e)))
                             (⊙ out> (fmt "   * %S\n" (car (tget.':log dat))))
                             (⊙ out> (fmt "\n")))))
                      (report-log-entries (ƛ (backup-buf)
                            (del-other-wins)
                            (let*(dat
                                  (dat<-read        (ƛ (buf) 
                                                       (! dat  (cond-case err
                                                                 (read buf)  
                                                                 (end-of-file '_eof_)))))
                                  (output-buf        (get-buffer-create "output"))
                                  (org-win           (cur-win))
                                  (out>              (ƛ (v)  (with-buf output-buf  (insert v))))
                                  (output-win        (split-win* nil 10)))
                              (with-win output-win     (switch-2-buf "output"))
                              (with-buf output-buf     (erase-buf))
                              (with-buf backup-buf     (goto (point-min)))
                              (while (not (eq? '_eof_  (⊙ dat<-read backup-buf)))
                                (let*((log    (tget ':log dat)))
                                  (when log
                                    (⊙ report-entry dat))))))))
                  (with-temp-buf
                   (insert-file-contents p)
                   (⊙ report-log-entries (cur-buf))))))
        ;; ---------------------------------------------------------------------
        (lst           (filt   backup-log?   (open-file-recs))))
    (cond
        ((> (len lst) 1)
            (error  (fmt ":d - more than 1 backup-log? open")))
        ((= (len lst) 1)     ;; use currently open BACKUP-LOG
           (let*((path      (tget. ':path (car lst))))
               (⊙ do-report path)))
        (t                   ;; not open - chk for assoc BACKUP-LOG under DEFAULT-DIRECTORY
          (let*((path     (str+ default-directory "BACKUP/BACKUP-LOG")))
            (cond
             ((file-exists? path)
                  (⊙ do-report path))
             (t  (error (fmt ":d - no BACKUP-LOG for [%S]" default-directory)))))))))

(defun random-ri ()
  ;;  1..26   : a-z
  ;; 27..52   : A-Z
  ;; 53..63   : 0-9
  (let*((from-range-az     (ƛ (i) (+ 97 i)))   ;; ?a  ->  ?z
        (from-range-AZ     (ƛ (i) (+ 65 i)))   ;; ?A  ->  ?Z
        (from-range-09     (ƛ (i) (+ 48 i)))   ;; ?0  ->  ?9
        (cnt 0)
        ∑)
    (while (<  (++ cnt)  5)
      (let*((v   (random 62))
            (c   (cond
                  ((and  (>= v 0)   (<= v 25))   (⊙ from-range-az  v))
                  ((and  (>= v 26)  (<= v 51))   (⊙ from-range-AZ  (- v 26)))
                  ((and  (>= v 52)  (<= v 61))   (⊙ from-range-09  (- v 52)))
                  (t   (error (fmt "opps - out of range [%S]" v))))))
        (push  (fmt "%c" c)  ∑)))
    (apply 'str+  ∑)))

(defun collect-functions (buf)
  (let*((read*  (ƛ () 
                   (cond-case err
                              (read (cur-buf))
                              (error nil)
                              nil)))
        ∑)
    (with-buf buf
    (save-excursion
      (let*((func?  (ƛ (dat)
                       (and (list? dat)
                            (not (null dat))
                            (eq? 'defun (car dat))))))
        (goto (pt-min))
        (while (not (eob?))
          (let*((dat   (⊙ read*)))
            (when (⊙ func? dat)    (push  (cadr dat)  ∑)))))))
    (nreverse ∑)))
;;/ Usage: 
;;    (defun x () (interactive)
;;       (dolist (e   (collect-functions (cur-buf)) )
;;         (ins (fmt  (str+ ";;" "|   %s\n") e))))

;;| </SECTION-4>

;;/==================================================================
;;/==================================================================
(provide 'dlm)
  
