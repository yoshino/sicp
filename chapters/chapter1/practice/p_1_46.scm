; sqrt
(define (sqrt x)
  (sqrt-iter 1.0 x))

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


; 関数の不動点を求める
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


; 問題はこの２つの手続きを抽象化して１つの式にできないか？ということ
(define (interactive-improve good-enough? improve)
  (define (iter-imp guess)
    (if (good-enough? guess)
      guess
      (iter-imp (improve guess))))
  iter-imp)

; sqrt
(define (sqrt x)
  ((interactive-improve (lambda (guess)
                          (< (abs (- (square guess) x)) 0.001))
                        (lambda (guess)
                          (average guess (/ x guess))))
   1.0))

;fixed-point
(define (fixed-point f first-guess)
  ((interactive-improve (lambda (guess)
                          (< (abs (- (f guess) guess)) 0.00001))
                        (lambda (guess)
                          (f guess)))
   first-guess))
