(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (expt x this-coeff) higher-terms))
              0
              coefficient-sequence))

;ex) (horner-eval 2 (list 1 3 0 5 0 1))
(define l (list 1 3 0 5 0 1))
