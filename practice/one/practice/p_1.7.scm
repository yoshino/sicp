; 数字が小さい値だと平方根の結果は等しくなってしまう

; 数字が大きい値だと平方根の値を計算するのに非常に時間がかかる

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (sqrt-iter 1.0 0 x))

(define (good-enough? guess previous-guess)
  (< (abs (- previous-guess guess)) 0.001))

(define (sqrt-iter guess previous-guess x)
  (if (good-enough? guess previous-guess)
    guess
    (sqrt-iter (improve guess x) guess x)))
