(define C ( make-connector))
(define F ( make-connector))
(celsius-fahrenheit-converter C F)

(define (celsius-fahrenheit-converter c f)
  (let ((u ( make-connector))
        (v ( make-connector))
        (w ( make-connector))
        (x ( make-connector))
        (y ( make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

; 3.37
; 上記の定義を簡略化しよう
(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5)) x) (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z) z))

(define (c- x y)
  (let ((z (make-connector)))
      (adder y z x))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)))
