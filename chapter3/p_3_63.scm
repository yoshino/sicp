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

; ストリームプロセスとしての反復の定式化

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

(define ss (sqrt-stream 2))

;gosh> (stream-head ss 10)
;1.0
;1.5
;1.4166666666666665
;1.4142156862745097
;1.4142135623746899
;1.414213562373095
;1.414213562373095
;1.414213562373095
;1.414213562373095
;1.414213562373095

; phai
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define (partial-sums stream)
  (stream-map accum stream))

(define (pi-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4)) ; 和に４をかけ合わせる

;gosh> (stream-head pi-stream 10)
;4.0
;2.666666666666667
;3.466666666666667
;2.8952380952380956
;3.3396825396825403
;2.9760461760461765
;3.2837384837384844
;3.017071817071818
;3.2523659347188767
;3.0418396189294032

; 収束を加速させるためにオイラーの加速
(define (euler-transform s)
  (let ((s0 ( stream-ref s 0)) ; S n−1
        (s1 ( stream-ref s 1)) ; S n
        (s2 ( stream-ref s 2))); S n+1
    (stream-cons (- s2 (/ ( square (- s2 s1 ))
                          (+ s0 (* -2 s1) s2 )))
                 (euler-transform (stream-cdr s)))))

(define euler-pi-stream (euler-transform pi-stream))

;gosh> (stream-head euler-pi-stream 10)
;3.166666666666667
;3.1333333333333337
;3.1452380952380956
;3.13968253968254
;3.1427128427128435
;3.1408813408813416
;3.142071817071818
;3.1412548236077655
;3.1418396189294033
;3.141406718496503

;------------------------------------------------
; 3.63
;------------------------------------------------
(define counter 0)
(define (count-up) (set! counter (+ counter 1)))
(define (count-reset) (set! counter 0))

(define (sqrt-improve guess x)
  (begin
    (count-up)
    (display counter)
    (newline)
    (average guess (/ x guess))))

(define (sqrt-stream x)
  (define guesses
    (stream-cons
      1.0
      (stream-map (lambda (guess) (sqrt-improve guess x))
                  guesses)))
  guesses)
(define good-s (sqrt-stream 2))

;gosh> (stream-head good-s 5)
;1.0
;1
;1.5
;2
;1.4166666666666665
;3
;1.4142156862745097
;4
;1.4142135623746899

; 局所変数を使わない方法
(define (bad-sqrt-stream x)
  (stream-cons
    1.0
    (stream-map (lambda (guess) (sqrt-improve guess x))
                (bad-sqrt-stream x))))

(define bad-s (bad-sqrt-stream 2))

;gosh> (stream-head bad-s 5)
;1.0
;1.5
;1.4166666666666665
;4
;5
;6
;1.4142156862745097
;7
;8
;9
;10
;1.4142135623746899

;状態変数に保存していないので毎回それまでの計算を行わなければならない。

;メモライズしなかった場合は、guesses を使う版でも繰り返しストリームが生成されるために二つの版の効率に違いはない。
