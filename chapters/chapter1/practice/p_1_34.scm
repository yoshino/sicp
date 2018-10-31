(define (f g) (g 2))

;gosh> (f f)

;*** ERROR: invalid application: (2 2)
;Stack Trace:
;_______________________________________
;  0  (for-each (lambda (e) (if (<condition> e) (report-error e) (b ...
;        at "/home/yoshino/.cache/nvim/dein/.cache/init.vim/.dein/autoload/gosh_repl/repl.scm":109
;  1  (read-eval-print-loop #f %repl-eval% (lambda args (for-each ( ...
;        at "/home/yoshino/.cache/nvim/dein/.cache/init.vim/.dein/autoload/gosh_repl/repl.scm":105

; (f f)
; (f 2)
; (2 2)
; と展開する。
; 最終的に(2 2)の左側は手続きではないのでエラーとなる。
