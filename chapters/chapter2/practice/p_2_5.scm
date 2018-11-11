; cons
(define (cons p q)
  (* (expt 2 p) (expt 3 q)))

; car
(define (car z)
  (car-iter z 0))
(define (car-iter z count)
  (if (= (remainder z (expt 2 count)) 0)
    (car-iter z (+ count 1))
    (- count 1)))

; cdr
(define (cdr z)
  (cdr-iter z 0))
(define (cdr-iter z count)
  (if (= (remainder z (expt 3 count)) 0)
    (cdr-iter z (+ count 1))
    (- count 1)))

; gosh> (cons 3 12)
; 4251528
; gosh> (car (cons 3 12))
; 3
; gosh> (cdr (cons 3 12))
; 12
