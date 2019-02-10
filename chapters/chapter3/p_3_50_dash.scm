(use math.prime)
(use util.stream)

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (stream-cons (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (stream-cons
        low
        (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (stream-cons (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define the-empty-stream '())

(define prime-stream
  (stream-car (stream-cdr (stream-filter miller-rabin-prime? (stream-enumerate-interval  10000 1000000)))))

;gosh> prime-stream
;10009
;gosh>  (stream-enumerate-interval  10000 1000000)
;#<promise(stream) 0x55c147406260>
;gosh> (stream-filter miller-rabin-prime? (stream-enumerate-interval  10000 1000000))
;#<promise(stream) 0x55c14740db80>
;gosh> (stream-car (stream-filter miller-rabin-prime? (stream-enumerate-interval  10000 1000000)))
;10007

;---------------------------------------------------------
; 3.51
;---------------------------------------------------------
(define ( display-stream s)
  (stream-for-each display-line s))
(define ( display-line x) (newline) (display x))
(define (show x)
  (display-line x) x)

(define x (stream-map show (stream-enumerate-interval 0 10)))

;gosh> (stream-ref x 5)
;0
;1
;2
;3
;4
;55
;gosh> (stream-ref x 7)
;6
;77

;---------------------------------------------------------
; 3.52
;---------------------------------------------------------
(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)

(define seq (stream-map accum
              (stream-enumerate-interval 1 20)))

;gosh> (stream-car seq)
;1
;gosh> sum
;1
;gosh> (stream-car (stream-cdr seq))
;3
;gosh> sum
;3


;(define y (stream-filter even? seq))
;(define
;  (stream-filter (lambda (x) (= ( remainder x 5) 0))
;                 seq))

;gosh> (stream-ref y 7)
;136
;gosh> (display-stream y)
;6
;10
;28
;36
;66
;78
;120
;136

;gosh> (display-stream z)
;10
;15
;45
;55
;105
;120
