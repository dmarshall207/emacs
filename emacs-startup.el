
;;// TEST  --   YES --  IT WORKS
;;(defvar lexical-binding t)
(setq-default lexical-binding t)   ;; ??????????????????
;;(setq lexical-binding t

;;(setq lexical-binding t)

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Closures.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Lexical-Binding.html#Lexical-Binding
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Using-Lexical-Binding.html


;;////////////////////////////////////////

;; to be/come aware of:

;;  cmd:  'completion-at-point : default:  M-tab

;;////////////////////////////////////////



;;https://www.emacswiki.org/emacs/PrefixKey
;;https://www.emacswiki.org/emacs/PrefixKeymaps
;;https://www.emacswiki.org/emacs/keymap-unset-key

;; http://ergoemacs.org/emacs/keyboard_shortcuts_examples.html

;; https://www.gnu.org/software/emacs/manual/elisp.html
;; "GNU Emacs Lisp Reference Manual"

;; https://www.gnu.org/software/emacs/manual/elisp.html
;; "GNU Emacs Lisp Reference Manual"

;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Installing-Texinfo-documentation.html
;; "3.5 How do I install a piece of Texinfo documentation?"


;; https://emacs.stackexchange.com/questions/7238/info-file-emacs-does-not-exist
;; "Info file emacs does not exist"
;;    * (:tag   key )
;;    * links to the info files:  'emacs.info' 'elisp.info'
;;        https://www.gnu.org/software/emacs/manual/info/emacs.info.gz
;;        https://www.gnu.org/software/emacs/manual/info/elisp.info.gz

;; https://help.ubuntu.com/community/EmacsHowto

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/The-Mark.html
;;     *  BINGO
;;     * (:tag   key)
;;     * info on 'mark'  --   'transient-mark-mode' /  'deactivate-mark'


;; https://emacs.stackexchange.com/questions/14199/optional-parameter-defaults
;;     * default values for arguments

;; . . . .  .

;; http://ergoemacs.org/emacs/emacs_hyper_super_keys.html

;; http://ergoemacs.org/emacs/emacs_region.html
;;            * DAVIS  check out this site
;;     * KEY

; https://www.gnu.org/software/emacs/manual/html_node/elisp/The-Mark.html

(global-unset-key (kbd "C-a"))

;;////////////////////////////////////////
;; find -name "*.el" -exec grep test {} \;

;; find  . -name "*.el" -print -exec fgrep hello {} \;
;;      * (:tag   KEY  LESSON)
;;      * from: https://unix.stackexchange.com/questions/12902/how-to-run-find-exec


;; see 'quoted-insert -- to genreate ctl char string for 'global-set-key

;;////////////////////////////////////////
;;/ :on-the-fly-setup

(setq truncate-lines t)
(global-set-key (kbd "<f1>")        'buffer-menu)
(global-set-key (kbd "<f11>")       'other-window)
(global-set-key (kbd "<f2>")        'other-window)
(global-set-key (kbd "C-v")         'yank)
(global-set-key (kbd "C-<delete>")  'kill-region)    ;; diff from std (delete-region)          
(global-set-key (kbd "M-R")         'point-to-register)
(global-set-key (kbd "M-r")         'jump-to-register)
(global-set-key (kbd "C-c") 'kill-ring-save)
;;(global-set-key (kbd "C-z") 'undo-ring-save)
(setq visible-bell 1)               ;; :d disable bell
(require 'dired-x)                  ;; see 'LESSONS'
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

;;.... level-2 .....
(show-paren-mode 1)                       ;; show matching paren's
;;(global-set-key (kbd "A-a") 'eval-defun)  ;; ????????
(global-set-key (kbd "M-]")        'eval-defun)
(global-set-key (kbd "M-\\")        'x)
(setq-default indent-tabs-mode nil)          ;; "no-tabs mode"

;;/////////////////////////////
;;/ :prefix key 'a' setup

(global-unset-key (kbd "C-a"))  ;; clear orig to create prefix key for ...
(global-set-key (kbd "C-a C-x")     'x)

;; ??? fail: 
;;(global-set-key (kbd "C-a C-a s") 'd-set-to-col)
;;(global-set-key (kbd "C-a C-a v") 'd-align-to-col)

;;/////////////////////////////

(add-to-list 'load-path "/sto/ff-cfg/emacs")

;; ===============================================================
;; intial grounding -- for sanity
(global-set-key (quote [f1]) 'buffer-menu)
(global-set-key "" (quote yank))  ;;; see 'quoted-insert -- to genreate ctl char string for 'global-set-key

;; ===============================================================
;; setup color scheme
             ;;|| see:  https://wikemacs.org/wiki/Zenburn-theme
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;;(add-to-list 'custom-theme-load-path "/sto/ff-cfg")
(add-to-list 'custom-theme-load-path "/sto/ff-cfg")
(load-theme 'zenburn t)

;; ==================================================================

(require 'defines1)   ;; generial / "aliases"
(require 'defines2)   ;; plist   -- ?? combine with define6 ?? 
(require 'defines3)   ;; windows / buffers / frames
(require 'defines4)   ;; text properties
(require 'defines5)   ;; system
(require 'defines6)   ;; "overflow" ?
(require 'defines7)
(require 'defines8)   ;; user interaction
(require 'font-lock-setup)
(add-hook 'scheme-mode-hook  (lambda ()
			       (d-load-scheme-font-lock-spec)
			       (initialize-for-scheme-editing)))
(require 'align-to-col)
(global-set-key (kbd "C-a C-s")     'd-set-to-col)
(global-set-key (kbd "C-a C-a")     'd-align-to-col)
(require 'dlm)
;;(require 'dlm2)

;; ================================================================
;;  to setup comment handling
(require 'comments)
(global-set-key      '[M-f11]     'd-ins-comm)       ; Alt-F11
(global-set-key      '[s-f11]     'd-rm-comm)        ; Win-F11
;(cond
;  ((equal 'gnu/linux system-type)
;      (global-set-key      '[s-f11]     'd-rm-comm))       ; Win-F11
;  ((equal 'windows-nt system-type) 
;       (global-set-key     '[C-f11]     'd-rm-comm) ))     ; Ctl-F11


;; ================================================================
;;  setup  'plate-util3

(require 'plate-util-setup)
(global-set-key (quote "\M-p") (quote d-insert-plate3))      ;A-p
(global-set-key (quote "\C-p") (quote d-select-plate-root)) ;C-p

;; ================================================================
;;  setup  'mlist

;; (require 'mlist "/sto/elisp/my-mods/my-code-edit/util.mlist/mlist.el")
 (require 'mlist "util.mlist/mlist.el")
;;    * see ":key-bindings"             - in 'mlist.el'

;; ====================================================
;; settings managed by emacs

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(truncate-lines t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; ====================================================
;;(global-set-key (quote [f12]) (quote call-last-kbd-macro))
(global-set-key (kbd "<f12>")       'call-last-kbd-macro)
(global-set-key (kbd "C->")         'indent-code-rigidly)
(global-set-key (kbd "C-d")         'keyboard-quit) ;; 'C-d' default is 'delete-char
;; ====================================================
(require 'my-edit-tools "./ubu-01.el")
(global-set-key (kbd "<C-s-right>")     'shift-right)   ;; 'M' = window key
(global-set-key (kbd "<C-s-left>")      'shift-left)
;; ====================================================
(require 'my-frame-tools "./ubu-02.el") ;; hi

