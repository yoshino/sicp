(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))

; gosh> (cons 3 4)
; #<closure ((cons cons) m)>
; gosh> (car  (cons 3 4))
; 3
; gosh> (cdr  (cons 3 4))
; *** ERROR: pair required, but got #<closure ((cons cons) m)>
; Stack Trace:
; _______________________________________
;   0  (for-each (lambda (e) (if (<condition> e) (report-error e) (b ...
;         at "/home/yoshino/.cache/nvim/dein/.cache/init.vim/.dein/autoload/gosh_repl/repl.scm":109
;   1  (read-eval-print-loop #f %repl-eval% (lambda args (for-each ( ...
;         at "/home/yoshino/.cache/nvim/dein/.cache/init.vim/.dein/autoload/gosh_repl/repl.scm":105

(define (cdr z)
  (z (lambda (p q) q)))

; gosh> (cdr  (cons 3 4))
; gosh> (cdr  (lambda (m) m 3 4))
