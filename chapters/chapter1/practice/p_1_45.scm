; 関数の繰り返し（ネスト)
; 関数の合成
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (compose-iter f n));

(define (compose-iter f n)
  (if (= n 1)
    f
    (compose-iter (compose f f) (- n 1))))

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

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (nth-root x n)
  (fixed-point
     (average-damp
        (lambda (y) (/ x (expt y (- n 1)))))
     1.0))

(define (nth-root x n)
  (fixed-point
     ((repeated average-damp 3)
        (lambda (y) (/ x (expt y (- n 1)))))
     1.0))
