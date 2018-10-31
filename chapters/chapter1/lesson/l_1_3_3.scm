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

; gosh> (fixed-point cos 1.0)
; 0.7390822985224024

; 不動点のイメージは以下がわかりやすかった
; http://www.wikiwand.com/ja/%E4%B8%8D%E5%8B%95%E7%82%B9
