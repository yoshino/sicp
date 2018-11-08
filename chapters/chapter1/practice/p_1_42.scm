; 関数の合成
(define (compose f g)
  (lambda (x) (f (g x))))

(define (inc x)
  (+ x 1))

; gosh> ((compose square inc) 6)
; 49
