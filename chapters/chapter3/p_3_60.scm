(use util.stream)

(define (stream-head s n)
  (define (iter s n)
    (if (<= n 0)
      'done
      (begin
        (display (stream-car s))
        (newline)
        (iter (stream-cdr s) (- n 1)))))
  (iter s n))

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1)) ; 無限の整数ストリーム

(define ones (stream-cons 1 ones))

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (integrate-series s)
  (div-streams s integers))

(define s1 (integrate-series ones))

(define exp-series
  (stream-cons 1 (integrate-series exp-series)))

(define cosine-series
  (stream-cons 1 (stream-map - (integrate-series sine-series))))

(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

;--------------------------------------------------------------------
; 3.60
;--------------------------------------------------------------------
(define (add-streams s1 s2) (stream-map s1 s2))

(define (mul-series s1 s2)
  (stream-cons 1 (add-streams s1 s2)))

(define (mul-series s1 s2)
  (stream-cons (* (stream-car s1)
                  (stream-car s2))
               (add-streams
                 (scale-stream (stream-cdr s1)
                               (stream-cdr s2))
                 (mul-series (stream-cdr s1) s2))))

(define square-sine-and-square-cosine
  (add-streams
    (mul-series sine-series sine-series)
    (mul-series cosine-series cosine-series)))


(define result (stream-head square-sine-and-square-cosine 10))


;--------------------------------------------------------------------
; 3.61
;--------------------------------------------------------------------
(define (invert-unit-series stream)
  (cons-stream 1
               (mul-series (scale-stream (stream-cdr s1) -1)
                           (invert-unit-series stream))))

;--------------------------------------------------------------------
; 3.62
;--------------------------------------------------------------------
(define (div-stream s1 s2)
  (if (= s2 0)
      (error "ZERO-DIVISOR" s2)
      (mul-streams s1
                   (invert-unit-series s2))))
