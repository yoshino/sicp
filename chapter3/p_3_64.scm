(use util.stream)

(define (average x y)
  (/ (+ x y) 2))

(define (stream-head s n)
  (define (iter s n)
    (if (<= n 0)
      'done
      (begin
        (display (stream-car s))
        (newline)
        (iter (stream-cdr s) (- n 1)))))
  (iter s n))

; sqrt
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (stream-cons
      1.0
      (stream-map (lambda (guess) (sqrt-improve guess x))
                  guesses)))
  guesses)

(define (sqrt x tolerance)
  (sqrt-limit (sqrt-stream x) tolerance))

(define (sqrt-limit stream tolerance)
  (define (iter s t)
    (if (> t (abs (- (stream-car s) (stream-car (stream-cdr s)))))
      (stream-car (stream-cdr s))
      (begin
        (display (stream-car s))
        (newline)
        (iter (stream-cdr s) t))))
  (iter stream tolerance))

;gosh> (sqrt 5 0.001)
;1.0
;3.0
;2.3333333333333335
;2.238095238095238
;2.236067977499978
