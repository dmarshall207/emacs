
(defun d-load-scheme-font-lock-spec ()  ;; called from 'scheme-mode-hook
  (interactive)
  (let*(  )
    (font-lock-add-keywords nil '(
       ("ᵈ"            0         'stand-out-green  prepend)
       ("ᶜ"            0         'font-lock-keyword-face  prepend)
       ("\\<def\\>\\|\\<defstx\\>\\|\\<def-vals\\>\\|\\<defmac\\>" 0       font-lock-keyword-face prepend)
       ("\\<define/\\w+\\>" 0       font-lock-keyword-face prepend)
       ("\\<defs\\>" 0              font-lock-keyword-face prepend)
       ("\\<def\\*\\>" 0            font-lock-keyword-face prepend)
       ("\\<def/pub\\>" 0              font-lock-keyword-face prepend)
       ("\\<def/pubment" 0              font-lock-keyword-face prepend)
       ("\\<def/pub-final" 0              font-lock-keyword-face prepend)
       ("\\<def/override" 0              font-lock-keyword-face prepend)
       ("\\<def/overment" 0              font-lock-keyword-face prepend)
       ("\\<def/override-final" 0              font-lock-keyword-face prepend)
       ("\\<def/augment" 0              font-lock-keyword-face prepend)
       ("\\<def/augride" 0              font-lock-keyword-face prepend)
       ("\\<def/augment-final" 0              font-lock-keyword-face prepend)
       (";\\.[ a-zA-Z].+" 0   'd-comment  prepend)          ;;/ :new
       ("/[-=]+"    0       'sep/hard prepend)
       ("|[-=]+"    0       'sep/soft prepend)
       (";/[ a-zA-Z].+" 0   'comment/hard prepend)
       (";|[ a-zA-Z].+" 0   'comment/soft prepend)
       (";-.*" 0   'keyword prepend)
       ("/[-=]+"    0       'sep/hard prepend)
       ("|[-=]+"    0       'sep/soft prepend)
       (";/[ a-zA-Z].+" 0   'comment/hard prepend)
       (";|[ a-zA-Z].+" 0   'comment/soft prepend)
       (";-.*" 0   'keyword prepend)
       ;;(";\\[[^(].*" 0          'global-var prepend)
       ;;(";\\+[^(].*" 0          'global-var prepend)
       (";\\+[^(].*" 0          'header-bar prepend)
       ("\\<define-package\\>" 0   font-lock-keyword-face prepend)
       ("\\<:dlm\\>" 0            'stand-out-green prepend)
       ("\\<:dm\\>" 0            'stand-out-green prepend)
       ("\\<:d\\>"  0            'stand-out-green prepend)
       ;; :dm - note 'dff' is setup in racket as a macro that is the same
       ;; as 'define' and 'def' -- done this way to allow special highlighting
       ("\\<dff\\>"      0     'font-lock-keyword-face  prepend) 
       ("\\<\\>"       0     'font-lock-function-name-face prepend)    ;;| loop alias
       ("\\<loop\\>"       0     'font-lock-function-name-face prepend)    ;;| loop alias
       ;; <function-name>
       ("\\<def\\> +(\\([:=ᶜ⨀⟐↗0-9A-Za-z+_∑/\\@?><&%.*-]+\\) *\\([^)]*\\)"      1     'zenburn-blue prepend)    ;;| func name
       ("\\<dff\\> +(\\([:=ᶜ⨀⟐↗0-9A-Za-z+_∑/\\@?><&%.*-]+\\) *\\([^)]*\\)"      1     'stand-out-magenta prepend)    ;;| func name
       ;; </function-name>
       ;; for non-pareth
       ("\\<dff\\> +\\([:ᶜ0-9A-Za-z+_∑/\\@?><&%.*-]+\\)"                    1     'stand-out-magenta prepend)
       ;; <function-args>
       ("\\<def\\> +(\\(ᶜ[:0-9A-Za-z+_∑/\\@?><&%.*-]+\\) +\\([^)]+\\)"      2     'zenburn-blue-2 prepend)  ;;| func args
       ;; </function-args>
       ;;| for class methods
       ;;("\\<define/[a-z]+\\> +(\\(\\w+\\) *\\([^)]*\\)" 1     'zenburn-blue )    ;;| func name
       ;;("\\<define/[a-z]+\\> +(\\(\\w+\\) +\\([^)]+\\)" 2     'zenburn-blue-2 prepend)  ;;| func args
       ("\\<define/[a-z]+\\> +(\\([:0-9A-Za-z+_∑/\\@?><&%.*-]+\\) *\\([^)]*\\)" 1     'zenburn-blue )    ;;| func name
       ;;("\\<define/[a-z]+\\> +(\\([:0-9A-Za-z+_∑/\\@?><&%.*-]+\\) +\\([^)]+\\)" 2     'zenburn-blue-2 prepend)  ;;| func args
       ("\\<def/[a-z]+\\> +(\\([:0-9A-Za-z+_∑/\\@?><&%.*-]+\\) *\\([^)]*\\)" 1     'zenburn-blue )    ;;| func name
       ;;("\\<def/[a-z]+\\> +(\\([:0-9A-Za-z+_∑/\\@?><&%.*-]+\\) +\\([^)]+\\)" 2     'zenburn-blue-2 prepend)  ;;| func args
       ("\\(\\<lets\\>\\) *("           1     'font-lock-keyword-face prepend)
       ("\\(\\<let\\*s\\>\\) *("        1     'font-lock-keyword-face prepend)
       ("\\(\\<let/\\>\\) *("        1     'font-lock-keyword-face prepend)  ;;/ :new
       ("\\<ƛ\\>" 0                                 'zenburn-blue prepend)
       ("\\<ƛ *(\\([^)]+\\)" 1                      'zenburn-blue-2 prepend)
       ("!" 0               'fg-DeepPink1 prepend)
       ;;("<\\([^/ ][0-9A-Za-z_/-]*\\)\\(.*\\)>.*" 0  'tag-open  prepend)   ;;/ good
       ;;("<\\([^ ][0-9A-Za-z_/-]*\\)\\(.*\\)>.*" 0  'tag-close  prepend)   ;;/ good
       ;;("♪" 0  'font-lock-builtin  prepend)
       ("♪" 0  'stand-out-green  prepend)
       ;;("• [a-zA-Z_-]+" 0         'stand-out-magenta prepend)
       ;;("• [[:word:]]+" 0         'stand-out-magenta prepend)
       ;;/ This is for my function header top-bar
       (";;[ \/][-=]+\\[\\([^]]*\\)\\][-=]+"  1       
            ;;'comment/hard
            ;;'fg-gray1                     ;;/ good
            ;;'gnus-cite-face-1               ;;/ good
            ;;'gnus-cite-face-4               ;;/ good
            ;;'gnus-group-mail-2              ;;/ maybe
            ;;'gnus-x-face                    ;;/ good
            ;;'hi-title                       ;;/ good
            'isearch                          ;;/ good
            prepend)
       (";;[ \/][-=]+\\[\\([^]]*\\)\\][-=]+[0]"  1
            'hi-4
            prepend)
       (";;[ \/][-=]+\\[\\([^]]*\\)\\][-=]+[1]"  1
            'gnus-x-face
            prepend)
       (";;[ \/][-=]+\\[\\([^]]*\\)\\][-=]+[2]"  1
            'fg-blue
            prepend)
       (";;[ \/][-=]+\\[\\([^]]*\\)\\][-=]+[3]"  1
            'zenburn-term-black
            prepend)
       (";;[ \/][-=]+\\[\\([^]]*\\)\\][-=]+[4]"  1
            'tag-open-name
            prepend)
       (";;[ \/][-=]+\\[\\([^]]*\\)\\][-=]+[5]"  1
            'hi-green
            prepend)
       (";;[ \/][-=]+\\[\\([^]]*\\)\\][-=]+[6]"  1
            'isearch-fail
            prepend)
       (";;[ \/][-=]+\\[\\([^]]*\\)\\][-=]+[7]"  1
            'hi-yellow
            prepend)
       (";;[ \/][-=]+\\[\\([^]]*\\)\\][-=]+[8]"  1
            'gnus-cite-7
            prepend)
       (";;[ \/][-=]+\\[\\([^]]*\\)\\][-=]+[9]"  1
            ;;'gnus-cite-11
            'jabber-title-small
            ;;'jabber-title-medium
            prepend)
       (";;[ \/][-=]+\\[\\([^]]*\\)\\][-=]+[a]"  1
            'cursor
            prepend)
       (";;[ \/][-=]+\\[\\([^]]*\\)\\][-=]+[b]"  1
            'emphasize-title-1
            prepend)


  ))))


(provide 'font-lock-setup)
