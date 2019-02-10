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

(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define (partial-sums stream)
  (stream-map accum stream))

; log2
(define (log2-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (log2-summands (+ n 1)))))

(define log2-stream
  (partial-sums (log2-summands 1)))

;gosh> (stream-head log2-stream 20)
;1.0
;0.5
;0.8333333333333333
;0.5833333333333333
;0.7833333333333332
;0.6166666666666666
;0.7595238095238095
;0.6345238095238095
;0.7456349206349207
;0.6456349206349207
;0.7365440115440116
;0.6532106782106782
;0.7301337551337552
;0.6587051837051838
;0.7253718503718505
;0.6628718503718505
;0.7216953797836152
;0.6661398242280596
;0.718771403175428
;0.6687714031754279

(define (euler-transform s)
  (let ((s0 ( stream-ref s 0))  ; S n−1
        (s1 ( stream-ref s 1))  ; S n
        (s2 ( stream-ref s 2))) ; S n+1
    (stream-cons (- s2 (/ ( square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform ( stream-cdr s )))))

(define euler-log-stream (euler-transform log2-stream))

;gosh> (stream-head euler-log-stream 20)
;0.7
;0.6904761904761905
;0.6944444444444444
;0.6924242424242424
;0.6935897435897436
;0.6928571428571428
;0.6933473389355742
;0.6930033416875522
;0.6932539682539683
;0.6930657506744464
;0.6932106782106783
;0.6930967180967181
;0.6931879423258734
;0.6931137858557215
;0.6931748806748808
;0.6931239512121866
;0.6931668512550866
;0.6931303775344023
;0.693161647077867
;0.6931346368409872



