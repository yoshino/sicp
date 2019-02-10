(define fact-res
  ((lambda (n)
   ((lambda (fact) (fact fact n))
    (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1))))))) 10))
;gosh> fact-res
;3628800

;a fib
(define fib-res
  ((lambda (n)
    ((lambda (fib)
       (fib fib n))
     (lambda (f n)
       (cond ((= n 0) 0)
             ((= n 1) 1)
             (else (+ (f f (- n 2)) (f f (- n 1)))))))) 10))
;gosh> fib-res
;55

;b
;ev? od? (- n 1)
;ev? od? (- n 1)
