; 抽象的に使う
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))


;(define (simpson f a b n)
;   (/ (* h (sum term 0 inc n)) 3))

;(define h (/ (- b a) n)) ; 定義より

;(define (inc n) (+ n 1)) ; 1つずつ増加させていく

;(define (y k)            ; yk = f(a + kh) なので
;  (f (+ a (* k h)))) 

;(define (term k)
;  (* (cond ((odd? k) 4)
;           ((or (= k 0) (= k n)) 1) ; 最初の項と最後の項
;           ((even? k) 2))
;     (y k)))


; 以上をまとめると
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (inc n) (+ n 1))
  (define (y k)
    (f (+ a (* k h)))) 
  (define (term k)
    (* (cond ((odd? k) 4)
         ((or (= k 0) (= k n)) 1)
         ((even? k) 2))
   (y k)))
  (/ (* h (sum term 0 inc n)) 3))

(define (cube n) (* n n n))

; gosh> (simpson cube 0 1 100)
; 1/4
; gosh> (simpson cube 0 1 1000)
; 1/4

; 別の近似と比べてもシンプソンの公式の方が優秀
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
             dx))
; gosh> (integral cube 0 1 0.01)
; 0.24998750000000042
; gosh> (integral cube 0 1 0.001)
; 0.249999875000001
; gosh> (integral cube 0 1 0.0001)
; 0.24999999874993412
