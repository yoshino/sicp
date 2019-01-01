(use util.stream)

(define counter 0)
(define (count-up)
  (set! counter (+ counter 1)))
(define (counte-reset)
  (set! counter 0))

(define (add x y)
  (count-up)
  (display counter)
  (newline)
  (+ x y))

(define (add-streams s1 s2)
  (stream-map add s1 s2))

(define fibs
  (stream-cons
    0
    (stream-cons 1
                 (add-streams (stream-cdr fibs) fibs))))

;gosh> (stream-ref fibs 3)
;1
;2
;2
;gosh> (stream-ref fibs 10)
;3
;4
;5
;6
;7
;8
;9
;55
;gosh> (stream-ref fibs 10)
;55
;gosh> (stream-ref fibs 11)
;10
;89

;呼び出される加算はn回。

;memo手続きによる加算を使わない場合

; 1 1 2 3 5
; の最後の5を求める場合memo化ができていれば、2 + 3 = 5
; できていない場合、
; 2を求めるのに、1 + 1
; 3を求めるのに、1 + 1 = 2、1 + 2 = 3 という計算が必要になる。
