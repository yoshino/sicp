; 再帰
(define (cont-frac n d k)
  (define (frac i)
    (if (< i k)
      (/ (n i) (+ (d i) (frac (+ i 1))))
      (/ (n i) (d i))))
  (frac 1))

; d: 1 3 5 7 .....
(define (d i)
  (- (* 2 i) 1))

; n: x * x
(define (n i)
  (* i i))


(define (tan-cf x k)
  (define (n k)
    (if (= k 1)
      x
      (- (square x))))
  (define (d k)
    (- (* 2 k) 1))
  (cont-frac n d k))

; gosh> (tan-cf 10.0 100.0)
; 0.6483608274590866
