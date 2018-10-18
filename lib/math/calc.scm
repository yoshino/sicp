; 階乗
(define (square x) (* x x))

; 絶対値
(define (abs x)
  (if (< x 0)
      (- x)
      x))

; 平方根
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; べき乗
(define (^ base exponent)
  (define (*^ exponent acc)
    (if (= exponent 0)
      acc
      (*^ (- exponent 1) (* acc base))))
  (*^ exponent 1))
