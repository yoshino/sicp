; 関数の繰り返し（ネスト)
; 関数の合成
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
    f
    (compose f (repeated f (- n 1)))))

(define (inc x) (+ x 1))

(define dx 0.00001)

; n重平滑化
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3.0)))

; fがネストで増加するなら数字は拡大していくし、
; ネストで減るなら減衰する。


