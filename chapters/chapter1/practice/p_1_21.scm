(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-devisor)
  (cond ((> (square test-devisor) n) n)
        ((divides? test-devisor n) test-devisor)
        (else (find-divisor n (+ test-devisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;practice
;gosh> (smallest-divisor 199)
;199
;gosh> (smallest-divisor 1999)
;1999
;gosh> (smallest-divisor 19999)
;7
