;-------------------------------------------------------------
; STREAM
;-------------------------------------------------------------
(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items ) (- n 1))))
(define (map proc items)
 (if (null? items )
      '()
      (cons (proc (car items)) (map proc (cdr items)))))
(define (scale-list items factor)
        (map (lambda (x) (* x factor)) items))
(define (add-lists list1 list2 )
         (cond (( null? list1 ) list2 )
               (( null? list2 ) list1 )
               (else (cons (+ (car list1 ) (car list2))
                           ( add-lists (cdr list1) (cdr list2))))))


; DRIVER-ROOP
gosh> (driver-loop)
;;; M-Eval input:
(define (cons x y) (lambda (m) (m x y)))

;;; M-Eval value:
ok

;;; M-Eval input:
(define (car z) (z (lambda (p q) p)))

;;; M-Eval value:
ok

;;; M-Eval input:
(define (cdr z) (z (lambda (p q) q)))

;;; M-Eval value:
ok

;;; M-Eval input:
(define (map proc items)
 (if (null? items )
      '()
      (cons (proc (car items)) (map proc (cdr items)))))

;;; M-Eval value:
ok

;;; M-Eval input:
(define st (cons x y))

;;; M-Eval value:
ok

;;; M-Eval input:
st

;;; M-Eval value:
(compound-procedure (m) ((m x y)) <procedure-env>)


; ストリーム化しなければエラー
gosh> (define st (cons x y))
*** ERROR: unbound variable: x
Stack Trace:
_______________________________________
  0  x
        [unknown location]
  1  (eval expr env)
        at "/usr/share/gauche-0.9/0.9.5/lib/gauche/interactive.scm":282
