;(if-fail (let ((x (an-element-of '(1 3 5))))
;           (require (even? x))
;           x)
;         'all-odd )
;=> all-odd

;(if-fail (let ((x ( an-element-of '(1 3 5 8))))
;           (require (even? x))
;           x)
;         'all-odd )
;=> 8

gosh> (load "./amb") 

;;; Amb-Eval input:
(define (require p)
  (if (not p) (amb)))

;;; Starting a new problem 
;;; Amb-Eval value:
ok

;;; Amb-Eval input:
(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

;;; Starting a new problem 
;;; Amb-Eval value:
ok

;;; Amb-Eval input:
(if-fail (let ((x (an-element-of '(1 3 5 8))))
            (require (even? x))
            x)
         'all-odd)

;;; Starting a new problem 
;;; Amb-Eval value:
8

;;; Amb-Eval input:
(if-fail (let ((x (an-element-of '(1 3 5 8))))
            (require (even? x))
            x)
         'all-odd)

;;; Starting a new problem 
;;; Amb-Eval value:
8

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
all-odd

;;; Amb-Eval input:
try-again

;;; There are no more values of
(if-fail (let ((x (an-element-of '(1 3 5 8)))) (require (even? x)) x) 'all-odd)
