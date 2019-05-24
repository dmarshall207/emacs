
;;| (:mri 1MdK7Sm96)
;;/ ==============================================================================
;;+        MLIST  --   ‶managed list″                                             
;;/ ==============================================================================
;;|  This is logic (devl) is to create a ‶managed list″ abstraction (MLIST).
;;|  
;;|  This idea is to use text properties to create a underling data struct
;;|  that is managed for (my) special editing purpose.
;;|  
;;|  Here the functionality provided (in part) is:
;;|      * Establish the "(" and ")" as identifiable delimeters.
;;|      * Provide a keymap for the speical property LOCAL-MAP that inables
;;|        the creation of special commands when the point is on the "(".
;;|           * like: Un/hide the contents of the list.
;;|  
;;|  #|  ;;/ @features:
;;|        * collapsible list  -- un/hide internals of a list
;;|  |#
;;| -----------------------------------------------------------------------------


;; aside:  the work I've been trying to remember:  IDIOM

;;/ :dm  CHECK OUT: 
;;/ node:   'Parsing Expressions'
;;   * These functions might simplify this logic.
;;   * Might provide ability to distinguish text inside a string/comment.

;;/ reminder-notes:
;;   there are 2 aspects/props that are in play:
;;      * INVISIBILITY, M-STATE
;;      * elisp LAST is fucked up

;;/ :design-issue:
;;    * should the closed mlist be made read-only


;;/ ==================================================================
;;/ :key-bindings : MLIST
(defvar mlist-prefix-map  (make-sparse-keymap "mlist command"))
(define-key   (current-global-map)   (kbd "M-m")      mlist-prefix-map)
(define-key   (current-global-map)   (kbd "M-m m")    '/make-mlist)
(define-key   (current-global-map)   (kbd "M-m u")    '/unmake-mlist)
;;node:  Key Binding Commands
;; -- Command: global-unset-key key
;;     This function removes the binding of KEY from the current global
;;     map.
;;     One use of this function is in preparation for defining a longer
;;     key that uses KEY as a prefix--which would not be allowed if KEY
;;     has a non-prefix binding.  For example:
;;
;;
;;node:  Help Summary
;;         'describe-bindings'


;;/ ==================================================================
;;/ devl / test data
;;(>>> 'clear)
(defvar test-data1 '( (aaaa  (bbbb b1  (cccc c1))  (dddd)) ))
(defvar test-data2 '(defun x () (interactive)   (let*() )))
(defvar test-data1 '(def (test a) (printf "> a [~s]\n" a)) )
(defvar test-data1 '(def (test1 a) 
                         (def (test2 b) 222)  222))
(defvar test-data1 '(defun test (&optional a) xxx ))
(defvar test-data1 '(def/pub (build spec) ))
(defvar test-data1 '(def aa 100))
(defvar test-data1 '(def (test)  (let*() )))
;;(/clear)
;;(>>> 'clear)
;;(>>> 'reset-count)
;;(! inhibit-read-only t)
;;(! inhibit-read-only nil)

;;/ ==================================================================
;;/ This section:
;;|   * /make-mlist
;;|   * setup-mlist
;;|   * mlist_toggle-open/closed
;;|   * mlist_set-open/closed
;;|   * determine-tagzones

;;/ ==================================================================
;;/ :setup-environment : MLIST 
(defvar mlist-begin-open-face       'open-mlist)
(defvar mlist-begin-closed-face     'closed-mlist)
;;(! buffer-invisibility-spec t)
;; note: BUFFER-INVISIBILITY-SPEC is set for each buffer -- see /MAKE-MLIST
;;(add-buf-invisi-spec '(mlist . t))   ;; format causes ellipses indicator of invisible text
;;/ ==================================================================
(defvar mlist-keymap
  ;;. Keymap used as  mlist LOCAL-MAP text property value which acts as 
  ;;. mlist managment commands ‶dispatch″.
  ;;. Function entries must be commands -- i.e. INTERACTIVE
  (let*((kmap  (make-sparse-keymap)))
    (suppress-keymap    kmap)
    (define-key  kmap  "\r"  'mlist_toggle-open/closed)
    (define-key  kmap  "u"   '/unmake-mlist)    
    ;;(define-key  kmap  "1"  'devl1)
    ;;(define-key  kmap  "2"  'devl2)
    kmap))

;;/ :idea : 2 step copy of range -- preserves the text properties
;;(defun /mark () (interactive)  ;; :dm  use this in MLIST-KEYMAP
;;  (! src-rng      (pt-list-range)))  ;;/ global
;;(defun /copy () (interactive)  (insert (buf (nth 0 src-rng) (nth 1 src-rng))))

(defun /make-mlist (&optional pos)  (interactive)
  ;; :dm fend to SETUP-MLIST
  ;;(>>> 'clear)
  (let*((setup-mlist  (ƛ (m-name m-keymap range)
           ;;. "M-NAME      symbol used for BUFFER-INVISIBILITY-SPEC
           ;;.  M-KEYMAP    value for LOCAL-KEYMAP prop"
           (let*((common-props      `(mlist ,m-name     rear-nonsticky t    read-only "oops - read-only"))
                 (setup-open-paren (ƛ (p)                    ;;. opening "("
                       (text-props+! p (+ p 1)    `(font-lock-face  ,mlist-begin-open-face
                                                    local-map       ,m-keymap
                                                    m-state         open
                                                    ,@common-props))))
                 (setup-close-paren (ƛ (p)                   ;;.  closing ")"
                       (text-props+! (- p 1) p    `(;;font-lock-face  ,mlist-end-face
                                                    ,@common-props))))
                 (setup-tagzone-props (ƛ (tagzone-ranges)
                       (dolist (r  tagzone-ranges)
                         (prop! (nth 0 r) (nth 1 r) 'tagzone t)))))
             (let*((tagzone-ranges     (determine-tagzones  (pt))))
               (cond
                (tagzone-ranges
                 (⊙ setup-open-paren   (nth 0 range))
                 (⊙ setup-close-paren  (nth 1 range))
                 (⊙ setup-tagzone-props tagzone-ranges))
                (t  (error (fmt "oops - invalid context - no TAGZONE matches")))))))))
  (let*((validate-context (ƛ ()
            (when (prop (pt) 'mlist)      (error (fmt ":d - oops - already an mlist")))))
        (insure-mlist-invisibility-spec (ƛ () 
              (when (not (member '(mlist . t) buffer-invisibility-spec))
                (add-buf-invisi-spec '(mlist . t))))))
    (let*((modified-state   (buffer-modified-p)))
      (cond
       (pos     (save-excursion 
                  (goto pos)
                  (let*((r       (pt-list-range)))
                    (⊙ validate-context)
                    (⊙ setup-mlist   'mlist  mlist-keymap  r)
                    (⊙ insure-mlist-invisibility-spec))))
       (t       (let*((r       (pt-list-range)))
                  (⊙ validate-context)
                  (⊙ setup-mlist   'mlist  mlist-keymap  r)
                  (⊙ insure-mlist-invisibility-spec)
                  (mlist_set-open/closed (pt) 'close))))
      (set-buffer-modified-p modified-state)))))

(defun /unmake-mlist () (interactive)
  (let*((validate-context (ƛ ()
           (when (not (prop (pt) 'mlist))      (error (fmt ":d - oops - not an mlist"))))))
    (let*((r       (pt-list-range))
          (b       (nth 0 r))
          (e       (nth 1 r))
          (modified-state   (buffer-modified-p)))
      (⊙ validate-context)
      (! inhibit-read-only t)
      (set-text-props  b  e  nil)
      (! inhibit-read-only nil)
      (set-buffer-modified-p modified-state))))

(defun mlist_toggle-open/closed ()  (interactive)  
  ;; used by MLIST-KEYMAP
  ;;(>>> 'clear)
  (let*((validate-context (ƛ ()
           (when (not (prop (pt) 'mlist))      (error (fmt ":d - oops - not on an mlist"))))))
    (⊙ validate-context)
    (let*((range    (pt-list-range))          ;; note: does validation also
          (b        (nth 0 range))            ;; begin
          (e        (nth 1 range))            ;; end
          (m-state  (prop b 'm-state))
          (modified-state   (buffer-modified-p)))
      (! inhibit-read-only t)
      (cond
       ((eq? 'open  m-state)     ;;(>>> (fmt "[---------closing]" ))
                                 (mlist_set-open/closed  range 'close))
       ((eq? 'closed  m-state)   ;;(>>> (fmt "[---------opening]" ))
                                 (mlist_set-open/closed  range 'open)))
      (! inhibit-read-only nil)
      (set-buffer-modified-p modified-state))))

(defun mlist_set-open/closed (mlist-location open/closed)
  "Assuming POINT is on a closed MLIST this marks the the list as open
   and ecursively decends nested list (tree) unhiding ranges that are 
   not marked as closed MLIST."
  (interactive)
  (let*((mlist-range       (if (list? mlist-location)
                               mlist-location
                             (save-excursion
                               (goto mlist-location)
                               (pt-list-range))))

        (open-mlist  (ƛ ()          ;; ini plate LIST.RECURSIVE-WALK_IN-BUF
          (let*((unhide-range (ƛ (rng)
                     (prop! (nth 0 rng) (nth 1 rng)  'invisible  nil)))
                (pt-sublist-ranges (ƛ ()                        ;; ‶point sublist ranges″
                     (let*((next-list-range (ƛ ()
                               (let*((next (ƛ () 
                                        (let*((p    (pt)))
                                          (let*((e      (forward-list)))
                                            (when (= (pt) p)  (error 'started-at-top-level))  ;; must have started at the top level
                                            (let*((b      (backward-list)))
                                              (goto e) `(,b ,e)))))))
                                 (condition-case  err   (⊙ next)
                                   (scan-error (progn '(goto p)  nil))
                                   (error (error (fmt ":d - oops - must have started at the top level ")))))))
                           r ∑)
                       (save-excursion
                         (forward-char)
                         (while  (! r  (⊙ next-list-range))  (push r ∑))
                         (reverse ∑)))))
                (sublist-ranges (ƛ (r)
                     (save-excursion
                       (goto (nth 0 r))
                       ;;(forward-char)
                       (⊙ pt-sublist-ranges))))
                (unhide-delim-parns (ƛ (r)                                 ;; unhide begin/end parenthesis
                    (⊙ unhide-range  `(,(nth 0 r) ,(1+ (nth 0 r))))        ;; "("
                    (⊙ unhide-range  `(,(nth 1 r) ,(1- (nth 1 r))))))      ;; ")"
                (closed-mlist?  (ƛ (r)
                    (and  (text-prop (nth 0 r) 'mlist)
                          (eq? 'closed (text-prop (nth 0 r) 'm-state)))))
                (loop (ƛ (ranges l)
                   (if (empty? ranges) 'done
                     (let*((r        (car ranges))
                           (childs   (⊙ sublist-ranges r))
                           ;;(r/str    (buf* (nth 0 r) (nth 1 r)))
                           )
                       (cond
                        ((⊙ closed-mlist? r)
                                 (⊙ unhide-delim-parns r)
                                 (⊙ expose-tagzones r)
                                 (⊙ loop  (cdr ranges)  l))
                        (t       (let*((comps      (inner-list-range-complements r)))
                                   ;;(dolist (r  comps) (>>> (fmt "      co [%S] [%S]" r (buf* (nth 0 r) (nth 1 r)))))
                                   ;;(when (> l 0)
                                   ;;  (dolist (r  childs)
                                   ;;  (>>> (fmt "      ch [%S] [%S]" r (buf* (nth 0 r) (nth 1 r))))))
                                   (mapc (ƛ (r)  (⊙ unhide-range r))  comps)
                                   (⊙ unhide-delim-parns r)
                                   (⊙ loop  childs        (1+ l))
                                   (⊙ loop  (cdr ranges)  l))))))))

                )
            (let*((p        (car mlist-range))
                  (lst      (inner-list-ranges  mlist-range))
                  (comps    (inner-list-range-complements mlist-range)))
              (! inhibit-read-only t)
              (props+! p (1+ p) `(m-state         open
                                  font-lock-face  ,mlist-begin-open-face))
              (⊙ expose-tagzones mlist-range)
              (⊙ loop `(,mlist-range) 0)
              (! inhibit-read-only nil)))))
        (expose-tagzones (ƛ (rng)  ;; ?? list range
              (let*((unhide-range    (ƛ (r) (prop! (nth 0 r) (nth 1 r)  'invisible  nil)))
                    (unhide-tagzones (ƛ (r) (unhide-matches r (ƛ (i) (prop i 'tagzone)))))
                    (tagzone-list?   ;; 1st char is tagzone => treat whole list as tagzone
                                     (ƛ (r) (prop (nth 0 r) 'tagzone))))
                (save-excursion
                  (goto (nth 0 rng))
                  (let*((lst     (pre-parse-range-categorization))
                        spc)
                    (while (! spc  (pop lst))
                        (let*((typ    (car spc))     ;; type
                              (r      (cadr spc)))   ;; range
                          (cond
                           ((eq? ':comp typ)             (⊙ unhide-tagzones r))
                           ((and (eq? ':list typ)        
                                 (⊙ tagzone-list? r))    (⊙ unhide-range r))))))))))
        (close-mlist (ƛ ()
             (let*((b     (nth 0 mlist-range))
                   (e     (nth 1 mlist-range)))
               (props+! b        (+ b 1)   `(m-state         closed
                                             font-lock-face  ,mlist-begin-closed-face))
               (prop!   (+ b 1)  (- e 1)   'invisible  'mlist)
               (⊙ expose-tagzones mlist-range))))
        (validate-context (ƛ () 
              (let*((on-open-parn?    (str=? "(" (buf* (pt) (1+ (pt))))))
                (when (not on-open-parn?)         (error (fmt ":d - X oops - not ON-OPEN-PARN?"))))
              (let*((valid-list?       (save-excursion   (condition-case err  (forward-list) (error  nil)))))
                (when (not valid-list?)           (error (fmt ":d - oops - not VALID-LIST?"))))
              (when (not (prop (pt) 'mlist))      (error (fmt ":d - oops - not on an mlist"))))))
    (save-excursion
      (goto (nth 0 mlist-range))
      (⊙ validate-context)
      (cond 
       ((eq? 'close open/closed)   ;;(>>> (fmt "[CLOSE]"))
               (! inhibit-read-only t)
               (when (not (eq? 'open (prop (pt) 'm-state)))   (error (fmt ":d - oops - not on a open mlist")))
               (⊙ close-mlist)
               (! inhibit-read-only nil))
       ((eq? 'open  open/closed)   ;;(>>> (fmt "[OPEN]"))
               (when (not (eq? 'closed (prop (pt) 'm-state))) (error (fmt ":d - oops - not on a closed mlist")))
               (⊙ open-mlist))))))

;;/ :dm this is a mixture of generialized and appl specific logic -- eventually
;;/ I need to seperate out these from each other.
(defun determine-tagzones (mlist-pos)
  (save-excursion
  (goto  (1+ mlist-pos))
  (let*((str->token-recs (ƛ (str offset)  ;;/ copy from T4
              (let*((type-tag (ƛ (e)
                        ;;(error (fmt "%s" e))
                        (cond
                         ((sym? e)  ':sym)
                         ((str? e)  ':str)
                         ((num? e)  ':num)
                         (t   (error (fmt "-oops - unhandled type"))))))
                    (read* (ƛ (s i)
                              ;; :dm consider case of 'nil' in the input
                              (condition-case err   (read-from-string s i)    (error  nil))))
                    (i       0)                ;; str index
                    (limit   (len str))
                    ∑)
                (while (< i limit)
                  (let*((r      (⊙ read* str i)))
                    (cond
                     (r        (let*((e      (cdr r))
                                     (tok    (car r))
                                     (sz     (len (fmt "%S" tok)))
                                     (b      (- e sz)))
                                 (let*((i*  (+ i offset))
                                       (b*  (+ b offset))
                                       (e*  (+ e offset)))
                                   (when (> b* i*)     (! ∑  `(,@∑   (:wsp  (,i* ,b*)))))
                                   (! i   e)
                                   ;;(! ∑  `(,@∑   (:tok (,(⊙ type-tag tok)  ,tok   (,b* ,e*)))))
                                   (! ∑  `(,@∑   (:tok  (,(⊙ type-tag tok)  ,tok  :rng (,b* ,e*)))))
                                   )))
                     (t        (! ∑  `(,@∑ (:wsp (,(+ i offset) ,(+ limit offset))))) ;; last whitespace
                               (! i limit)))))
                ∑)))
        (tokenize-ranges (ƛ (range-recs)
            (let*((token-recs (ƛ (b e)
                      ;;| :dm note: 'tokens' here are the not standard definition -- list are ‶tokens″
                      (let*((s           (buf* (nth 0 r) (nth 1 r)))
                            ;;(X           (error (fmt "%s" s))) 
                            (toks        (⊙ str->token-recs s b)))
                        toks)))
                  (is-list?        (ƛ (spec)  (memq  (car spec) '(:list :mlist))))
                  ∑)
              (dolist (spec range-recs)
                (let*((rng    (nth 1 spec)))
                  (cond
                   ((⊙ is-list? spec)       ;; list as ‶token″
                              (let*((rec    `(,(car spec) t  :rng  ,(cadr spec)))) ;; make proper property list
                                (! ∑     `(,@∑ (:tok ,rec)))))
                   (t         (let*((r              (nth 1 spec))
                                    (token-recs*    (⊙ token-recs  (nth 0 r) (nth 1 r))))
                                (! ∑     `(,@∑ ,@token-recs*)))))))
              ∑))))
    (let*((range-recs     (pre-parse-range-categorization mlist-pos)))    ;; ranges: list + compliment          
          ;;(dolist (rsec  range-recs) ;; components          ;;/ :dm keep devl / debug
          ;;  (let*((rng     (nth 1 rec)))
          ;;    (>>> (fmt "rec [%S] - [%S]" rec  (buf* (nth 0 rng) (nth 1 rng)))) ))
          ;;(>>> (fmt "---" ))          
      (let*((parse-recs       (⊙ tokenize-ranges range-recs)))
          ;;(dolist (r  parse-recs)                          ;;/ :dm keep this devl / debug loop
          ;;  (cond
          ;;   ((memq (car r)  '(:tok :tokA :tokL))    ;;(>>> (fmt "[TOKEN]" ))
          ;;               (let*((rng   (car (last (nth 1 r)))))
          ;;                 (>>> (fmt "[%S] - [%S]" r (buf* (nth 0 rng) (nth 1 rng))))))
          ;;   ((memq (car r)  '(:wsp :wsp1 :wsp2 ))   ;;(>>> (fmt "[WSPACE]" ))
          ;;               (let*((rng    (nth 1 r)))
          ;;                 (>>> (fmt "[%S] - [%S]" r (buf* (nth 0 rng) (nth 1 rng))))))))
          (let*((make-tok-map-vector (ƛ () 
                     (let*((i 0)  ∑)
                       (dolist (r  parse-recs)
                         (when (eq? ':tok (nth 0 r))   (push i  ∑))
                         (! i  (1+ i)))
                       (vconcat (reverse ∑)))))
                (parse-recs/v      (vconcat parse-recs))
                (tok-index-map     (⊙ make-tok-map-vector))  ;; maps :TOK items in PARSE-RECS/V
                (c/tok-n (ƛ (index-map/v)
                       (let*((tok-map-size       (len tok-index-map)))
                         (eval `(ƛ (n) (if (< n ,tok-map-size)
                                           (cadr (aref parse-recs/v  (aref ,index-map/v n)))
                                         nil)))))))
            ;;--------- appl specific logic: ------------
            ;;-- :dm this is where to add new mlist ‶types″ to be recognized
            (let*((tok-n*  (⊙ c/tok-n  tok-index-map))
                  (tok0    (⊙ tok-n* 0))
                  (tok1    (⊙ tok-n* 1))
                  (tok2    (⊙ tok-n* 2)))
              ;;(>>> (fmt "tok0 [%S]" tok0))
              ;;(>>> (fmt "tok1 [%S]" tok1))
              (let*((func-define?/type-1  (ƛ (tok0 tok1) ;; type-1 => SYM unambiguously means this is a function/method defintion
                        (let*((def?  (ƛ (sym)
                                 (not (not (memq sym
                                     '(def/pub def/pubment def/pub-final def/override
                                        def/overment def/override-final def/augment
                                        def/augride def/augment-final def/pub
                                        define/override defstx
                                        )))))))
                          (and tok0 tok1
                               (eq? ':sym  (nth 0 tok0))
                               (⊙  def?  (nth 1 tok0))) )))
                    (func-define?/type-2  (ƛ (tok0 tok1)  ;;/ not finished
                         (let*((def?  (ƛ (sym) (not (not (memq sym '(def)))))))
                           (and tok0 tok1
                                (eq? ':sym  (nth 0 tok0))
                                (⊙  def?  (nth 1 tok0))))))
                    (func-define?/type-3  (ƛ (tok0 tok1)
                         (let*((def?  (ƛ (sym) (not (not (memq sym '(defun)))))))
                           (and tok0 tok1
                                (eq? ':sym  (nth 0 tok0))
                                (⊙  def?  (nth 1 tok0))))))
                    (simple-tag? (ƛ (tok0)
                         (eq? ':sym  (nth 0 tok0)))))
                (cond
                 ((⊙ func-define?/type-1  tok0 tok1)       ;;(>>> (fmt "[func/type-1]" ))
                          `((,(nth 0 (pget tok0 ':rng))
                             ,(nth 1 (pget tok1 ':rng)))))
                 ((⊙ func-define?/type-2  tok0 tok1)       ;;(>>> (fmt "[func/type-2]" ))
                          `((,(nth 0 (pget tok0 ':rng))
                             ,(nth 1 (pget tok1 ':rng)))))
                 ((⊙ func-define?/type-3  tok0 tok1)       ;;(>>> (fmt "[func/type-3]" ))
                          `((,(nth 0 (pget tok0 ':rng))
                             ,(nth 1 (pget tok2 ':rng)))))
                 ((⊙ simple-tag? tok0)                     ;;(>>> (fmt "[simp]" ))
                          `(,(pget tok0 ':rng)))
                 (t                                        ;;(>>> (fmt "[else]" ))
                          nil))))))))))

;;/ ==================================================================
;;/ :aux : MLIST

;;/ this section:
;;|   * pre-parse-range-categorization
;;|   * unhide-matches
;;|   * pt-list-range
;;|   * inner-list-ranges
;;|   * inner-list-range-complements
(defun pre-parse-range-categorization (&optional from)
  (let*((next-list-range (ƛ ()
           (let*((next (ƛ () 
                     (let*((p    (pt)))
                       (let*((e      (forward-list)))
                         (when (= (pt) p)  (error 'started-at-top-level))
                         (let*((b      (backward-list)))
                           (goto e) `(,b ,e)))))))
             (condition-case  err   (⊙ next)
               (scan-error (progn '(goto p)  nil))
               (error (error (fmt ":d - oops - must have started at the top level "))))))))
    (save-excursion
      (cond
       ((and from (int? from))           (goto from))
       ((and from (list? from))          (goto (nth 0 from))))
      (let*(∑
            (top-r        (pt-list-range)))
        (forward-char)
        (let*((i        (pt))
              (more?    t))
          (while more?
            (let*((r         (⊙ next-list-range)))
              ;;(>>> (fmt "r [%S]" (buf* (nth 0 r) (nth 1 r)) ))
              (cond
               (r          (let*((b    (nth 0 r)))
                             ;;(>>> (fmt "1> r [%S]" (buf* (nth 0 r) (nth 1 r)) ))
                             (when (< i b)        ;; :COMP = compliment range -- ie non-list range
                                                  (push `(:comp (,i ,(nth 0 r)))   ∑))
                             (cond
                              ((prop b 'mlist)    (push `(:mlist ,r) ∑))
                              (t                  (push `(:list ,r)  ∑)))
                             (! i  (+ 0 (nth 1 r)))))
               (t          (let*((e    (1- (nth 1 top-r))))
                             (when (< i e)        (push  `(:comp (,i ,e))  ∑)))
                           (! more?  nil))))))
        (reverse ∑)))))
(defun unhide-matches (rng match?)
  (let*((i       (nth 0 rng))
        (limit   (nth 1 rng)))
    (while (< i limit)
      (when (⊙ match? i)   (rem-props i (1+ i) '(invisible)))
      (! i  (1+ i)))))
(defun pt-list-range ()
  "When POINT is on opening parenthesis and it defines a valid list
   the range is returned."
  (let*((b         (point)))
    (when  (not  (str=? "(" (buf* b (1+ b))))
      (error (fmt ":d - oops - PT-LIST-RANGE - not ON-OPEN-PARN?")))
    (let*((e      (save-excursion
                    (condition-case err  (forward-list) (scan-error  nil)))))
      (when (not e)   (error (fmt ":d - oops - not VALID-LIST?")))
      `(,b ,e))))
;;<group> ------------------------------------------------------------------
;;| logic to return:
;;|        list ranges           : INNER-LIST-RANGES
;;|        ranges between list   : INNER-LIST-RANGE-COMPLEMENTS
(defun inner-list-ranges (top-range &optional want?)   ;;/ ini copy of INNER-LIST-RANGES
  "This returns ranges of inner (next level) list w/in the TOP-RANGE list"
  (let*((inner-list-ranges (ƛ ()
           (let*((forward-list* (ƛ ()
                      (let*((p    (pt)))
                        (condition-case err
                            (let*((e      (forward-list))
                                  (b      (backward-list)))
                              (goto e)   `(,b ,e))          ;; return: range of list
                          (scan-error   (progn '(goto p)  nil))))))
                 r ∑)
             (cond
              (want?
                  (while  (! r  (⊙ forward-list*))  
                    (when (⊙ want? r)  (push r ∑))))
              (t  (while  (! r  (⊙ forward-list*))  (push r ∑))))             
             (reverse ∑)))))
    (save-excursion
      (goto  (1+ (nth 0 top-range)))
      (⊙ inner-list-ranges))))
(defun inner-list-range-complements (top-range)
  ;; depends on:  INNER-LIST-RANGES
  "This returns ranges w/in TOP-RANGE list that are not inner (next level) list."
  (let*((inner-beg       (1+ (nth 0 top-range)))    ;; first char inside TOP-RANGE
        (inner-end       (1- (nth 1 top-range)))    ;; last char inside TOP-RANGE
        (loop (ƛ (lst i ∑) 
             (if (null lst) ∑
               (let*((r    (car lst))               ;; range of list
                     (r*   `(,i   ,(nth 0 r))))     ;; corresponding compliment range
                 (⊙ loop  (cdr lst)   (nth 1 r)   (cons r* ∑)))))))
    (save-excursion
      (goto   inner-beg)
      (let*((list-ranges             (inner-list-ranges  top-range)))
        (cond
         ((null list-ranges)
               `((,inner-beg ,inner-end)))
         (t     (let*((∑                       (⊙ loop  list-ranges   inner-beg  '()))
                      (last-list-rng           (car (last list-ranges))))
                  (when last-list-rng
                    (let*((last-compliment-rng     `(,(nth 1 last-list-rng)
                                                     ,inner-end)))
                      (push    last-compliment-rng    ∑)))
                  (reverse ∑))))))))
;;</group>

;;/ ==================================================================

;;/ ==================================================================
;;/ :aux :  devl

(defun /clear-props () (interactive)
  (let*((modified-state   (buffer-modified-p))
        (b                (pt-min))
        (e                (pt-max)))    
    ;;(remove-text-properties b e '(here invisible))
    (! inhibit-read-only t)    
    (set-text-props  (pt-min)  (pt-max)  nil)             ;;/ remove ‶all″ props
    ;;(rem-range-invisi (pt-min)  (pt-max))
    (! inhibit-read-only nil)
    (set-buffer-modified-p modified-state)))
(defun /s () (interactive)  ;;/ show
  (>>> (fmt "buffer-invisibility-spec [%S]\n" buffer-invisibility-spec)))
(defun post-cmd ()    ;;/ show text props
  (let*((cmd/k    (this-command-keys))
        (cmd/v    (this-command-keys-vector)))
    (p (fmt "[%S] [%S]" (pt) (text-props (pt))))))
;;(defun / ()   (interactive) (add-hook    'post-command-hook  'post-cmd   nil  t))
(defun // ()  (interactive) (remove-hook 'post-command-hook  'post-cmd   t))

;;/ ==================================================================
(provide 'mlist)
