;---------------------------
;木の再帰：フィボナッチ
;---------------------------
; 反復
(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
    b
    (fib-iter (+ a b) a (- count 1))))

; 以下のように展開する

; (fib 4)
; (fib-iter 1 0 4)
; (fib-iter 1 1 3)
; (fib-iter 2 1 2)
; (fib-iter 3 2 1)
; (fib-iter 5 3 0)

; 上記は以下のように変換されている
; a -> a + b
; b -> a

; 今回は、
; a -> (b * q) + (a * q) + (a * p)
; b -> (b * p) + (a * q)
; の変換について考える

; ここで、p=0, q=1の時、

; a -> b + a
; b -> a

; となり一番上で定義した再帰の式と同じになる。


(define (newfib n)
  (newfib-iter 1 0 0 1 n))

(define (newfib-iter a b p q count)
  (if (= count 0)
    b
    (newfib-iter (+ (* b q) (* a q) (* a p)) 
              (+ (* b p) (* a q))
              p
              q
              (- count 1))))




; １回あたりの計算量を増やして速くする
(define (fib n)
   (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
   (cond ((= count 0) b)
         ((even? count)
          (fib-iter a
                    b
                    (+ (* p p) (* q q))     ; compute p' 穴埋め部分
                    (+ (* 2 p q) (* q q))   ; compute q'
                    (/ count 2)))
         (else (fib-iter (+ (* b q) (* a q) (* a p))
                         (+ (* b p) (* a q))
                         p
                         q
                         (- count 1)))))

;第一項
;a1 = bq + aq + ap
;b1 = bp + aq

;第二項
;a2 = b1q + a1q + a1p
;b2 = b1p + a1q

;第二項に第一項を代入すると、
;a2 = (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;b2 = (bp + aq)p + (bq + aq + ap)q

;恒等式を利用して解く

;b2に関して
;b2 = bp' + aq'

;b2 = (bp + aq)p + (bq + aq + ap)q
;= (bpp + apq) + (bqq + aqq + apq)
;= bpp + apq + bqq + aqq + apq
;= (bpp + bqq) + (2apq + aqq)
;= b(pp + qq) + a(2qp + qq)

; よって
; p' = p2 + q2
; q' = 2pq + q2

; a2に関しても同様の解が得られるのでこれが正解。



