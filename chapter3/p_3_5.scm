; 数学的に乱数っぽいのを算出している。
; 実際は全くランダムではない
; moduloは余りを求めているだけ
(define (rand-update x)
  (modulo (+ (* 214013 x) 253011) 32767))

(define random-init 100)
(define rand (let ((x random-init))
               (lambda ()
                 (set! x (rand-update x))
                 x)))

;gosh> (rand)
;28091
;gosh> (rand)
;3034
