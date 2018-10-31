; 関数の不動点を求める
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (golden-ratio x)
  (+ 1 (/ 1 x)))

; gosh> (fixed-point golden-ratio 10)
; 6477/4003
; 1.6180364726455159

; gosh> (fixed-point golden-ratio 3)
; 2207/1364
; 1.6180351906158357

; gosh> (fixed-point golden-ratio 0.1)
; 1.6180365296803654

; gosh> (fixed-point golden-ratio 1)
; 987/610
; 1.618032786885246
