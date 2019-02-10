;ambの評価機を以下のように書き直した
; analyze-ambをoverrideした

(use gauche.sequence)

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (set! choices (shuffle choices))
        (if (null? choices)
        (fail)
        ((car choices)
         env
         succeed
         (lambda () (try-next (cdr choices))))))
    (try-next cprocs))))


;;; Amb-Eval input:
(amb 1 2 3 4 5 6 7 8 9 10)

;;; Starting a new problem
;;; Amb-Eval value:
9

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
4

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
2

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
7

