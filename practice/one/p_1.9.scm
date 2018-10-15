(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define (+ a b)
  (if (= a 0) b (inc (+ (dec a) b))))

;(+ 3 3)
;(inc(+ 2 3))
;(inc(inc(+ 1 3)))
;(inc(inc(inc(+ 0 3))))
;(inc(inc(inc(3))))

;以上の結果より再帰プロセスであることがわかる。

(define (+ a b)
  (if (= a 0) b (+ (dec a) (inc b))))

;(+ 3 3)
;(+ 2 4)
;(+ 1 5)
;(+ 0 6)

;以上の結果より反復プロセスであることがわかる。
