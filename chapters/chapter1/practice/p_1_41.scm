(define (inc x) (+ x 1))

(define (double f)
  (lambda (x) (f (f x))))

; ((double inc) 3)
; 5

; (((double (double double)) inc) 5)
; 21

; 2 * 2 * 2 = 8 倍ではなくて
; 2**2)**2)**2) = 16 倍で展開する
