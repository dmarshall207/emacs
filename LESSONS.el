
;; ==================================================================
;; site:     www.emacswiki.org
;;             * :d - great site

;; ==================================================================
;; see:  https://www.emacswiki.org/emacs/DiredOmitMode
;;       "DiredOmitMode"

;; * for older versions:
        (add-hook 'dired-load-hook '(lambda () (require 'dired-x))) ; Load Dired X when Dired is loaded.
        (setq dired-omit-mode t) ; Turn on Omit mode.
;; * "most recent Emacs versions no hooks are needed"
        (require 'dired-x)
        (setq-default dired-omit-files-p t) ; Buffer-local variable
        (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

;; ==================================================================
;; How to specify a dir using 'require
;;       (require 'mlist "/sto/elisp/my-mods/my-code-edit/util.mlist/mlist.el")


;; ==================================================================
;; remove dired inserted subdirs

https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired-Updating.html

  * Place cursor on the subdir header line
  * cmd:  C-u k
