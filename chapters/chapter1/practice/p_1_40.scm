(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

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

(define (newton-transform  g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

; gosh> (newton-method (cubic 3 -2.4 6) 1)
; -3.9813366488305104

; 展開
; (fixed-point (newton-transform (lambda (x) (+) 1)
; (fixed-point (newton-transform (+ (* x x x) (* a x x) (* b x) c)) 1)

