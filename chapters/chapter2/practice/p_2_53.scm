(define a 1)
(define b 2)

;gosh> (+ a b)
;3

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

gosh> (list 'a 'b 'c)
(a b c)
gosh> (list a b c)
*** ERROR: unbound variable: c
Stack Trace:
_______________________________________
  0  (for-each (lambda (e) (if (<condition> e) (report-error e) (b ...
        at "/home/yoshino/.cache/nvim/dein/.cache/init.vim/.dein/autoload/gosh_repl/repl.scm":109
  1  (read-eval-print-loop #f %repl-eval% (lambda args (for-each ( ...
        at "/home/yoshino/.cache/nvim/dein/.cache/init.vim/.dein/autoload/gosh_repl/repl.scm":105
gosh> (list (list 'george))
((george))
gosh> (cdr '((x1 x2) (y1 y2)))
((y1 y2))
gosh>  (cadr '((x1 x2) (y1 y2)))
(y1 y2)
gosh> (pair? (car '(a short list)))
#f
gosh> (memq 'red '((red shoes) (blues socks)))
#f
gosh>  (memq 'red '(red shoes blues socks))
(red shoes blues socks)
