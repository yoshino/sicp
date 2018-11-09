; 関数の繰り返し（ネスト)
; 関数の合成
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (compose-iter f n))

(define (compose-iter f n)
  (if (= n 1)
    f
    (compose-iter (compose f f) (- n 1))))

; gosh> ((repeated square 1) 2)
; 4
; gosh> ((repeated square 2) 2)
; 16
; gosh> ((repeated square 2) 5)
; 625
def
