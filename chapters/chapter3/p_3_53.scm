(use math.prime)
(use util.stream)

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1)) ; 無限の整数ストリーム

(define (divisible? x y) (= ( remainder x y) 0))
(define no-sevens
  (stream-filter ( lambda (x) (not ( divisible? x 7)))
                 integers ))

;gosh> (stream-ref no-sevens 100)
;117

; フィボナッチの無限ストリーム
(define (fibgen a b) (stream-cons a ( fibgen b (+ a b))))
(define fibs (fibgen 0 1))

; 素数の無限ストリーム
(define (sieve stream)
  (stream-cons
    (stream-car stream)
    (sieve (stream-filter
             (lambda (x)
               (not (divisible? x (stream-car stream))))
             (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

;gosh> (stream-ref primes 0)
;2
;gosh> (stream-ref primes 1)
;3
;gosh> (stream-ref primes 2)
;5
;gosh> (stream-ref primes 50)
;233

; ストリームの暗黙定義

;ストリーム同士の合成
(define ones (stream-cons 1 ones))

;gosh> (stream-ref ones 10)
;1
;gosh> (stream-ref ones 1000)
;1

(define (add-streams s1 s2) (stream-map + s1 s2))
(define integers
  (stream-cons 1 (add-streams ones integers)))
;gosh> (stream-ref integers 0)
;1
;gosh> (stream-ref integers 99)
;100

;ストリームの定数をかける
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (stream-cons 1 (scale-stream double 2)))

;gosh> (stream-ref double 0)
;1
;gosh> (stream-ref double 1)
;2
;gosh> (stream-ref double 2)
;4
;gosh> (stream-ref double 3)
;8

;-------------------------------------------------
; 3.53
;-------------------------------------------------
(define (add-streams s1 s2) (stream-map + s1 s2))
(define s (stream-cons 1 (add-streams s s)))

; 1 2 4 8 16 32......

;gosh> (stream-ref s 0)
;1
;gosh> (stream-ref s 1)
;2
;gosh> (stream-ref s 2)
;4
;gosh> (stream-ref s 3)
;8
;gosh> (stream-ref s 4)
;16
;gosh> (stream-ref s 5)
;32
;gosh> (stream-ref s 6)
;64

;-------------------------------------------------
; 3.54
;-------------------------------------------------
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define factorials
  (stream-cons 1 (mul-streams
                   factorials
                   (add-streams ones integers))))

;(define s (add-streams ones integers))
;gosh> (stream-ref s 0)
;2
;gosh> (stream-ref s 1)
;3
;gosh> (stream-ref s 2)
;4
;gosh> (stream-ref s 3)
;5

;-------------------------------------------------
; 3.55
;-------------------------------------------------
(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)

;gosh> (stream-ref integers 0)
;1
;gosh> (stream-ref integers 1)
;2
;gosh> (stream-ref integers 2)
;3

(define (partial-sums stream)
  (stream-map accum stream))

;gosh> (stream-ref ss 0)
;1
;gosh> (stream-ref ss 1)
;3
;gosh> (stream-ref ss 2)
;6
;gosh> (stream-ref ss 3)
;10


