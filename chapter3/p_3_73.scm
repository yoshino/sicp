(use util.stream)

(define (add-streams s1 s2) (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (stream-head s n)
  (define counter 0)
  (define (iter s)
    (if (<= n counter)
      'done
      (begin
        (display (stream-car s))
        (newline)
        (set! counter (+ counter 1))
        (iter (stream-cdr s)))))
  (iter s))

(define ones (stream-cons 1 ones))

(define (integral integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)


(define (RC R C dt)
  (define (rc i vo)
    (add-streams
      (scale-stream i R)
      (integral (scale-stream i (/ 1 C)) vo dt)))
  rc)

(define RC1 ((RC 5 1 0.5) ones 0)) ;電流１が流れているケース

;gosh> (stream-head RC1 10)
;5
;5.5
;6.0
;6.5
;7.0
;7.5
;8.0
;8.5
;9.0
;9.5
;done




