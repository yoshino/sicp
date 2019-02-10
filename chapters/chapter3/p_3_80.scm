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

; 通常の微分
(define (integral integrand initial-value dt)
  (define int
    (stream-cons initial-value
               (add-streams (scale-stream integrand dt)
                            int)))
  int)

;-------------------------------------------------
; 3.80
;-------------------------------------------------
;(define (RC R C dt)
;  (define (rc i vo)
;    (add-streams
;      (scale-stream i R)
;      (integral (scale-stream i (/ 1 C)) vo dt)))
;  rc)
;(define RC1 ((RC 5 1 0.5) ones 0)) ;電流１が流れているケース

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (if (stream-null? delayed-integrand)
                   the-empty-stream
                   (let ((integrand (force delayed-integrand)))
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))


; 今回の問題
(define (RLC R L C dt)
  (define (rlc v0 i0)

    (define il (integral (delay dil) i0 dt))
    (define dil
      (add-streams
        (scale-stream vc (/ 1 L))
        (scale-stream il (- (/ R L)))))
    (define vc (integral (delay dvc) v0 dt))
    (define dvc
      (scale-stream il (/ -1 C)))

    (define (merge-stream s1 s2)
       (cons-stream (cons (stream-car s1) (stream-car s2))
                    (merge-stream (stream-cdr s1) (stream-cdr s2))))

    (merge-stream vc il))

  )

; 動かない😱
