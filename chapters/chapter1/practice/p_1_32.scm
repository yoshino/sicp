;---------------------------
; 再帰
;---------------------------

; 足し算
;(define (sum term a next b)
;  (if (> a b)
;    0
;    (+ (term a)
;       (sum term (next a) next b))))

; 掛け算
;(define (product term a next b)
;  (if (> a b)
;    1
;    (* (term a)
;       (product term (next a) next b))))

; 上記の足し算も掛け算ももう１段階抽象化をして、
; １つにまとめることができる
(define (accumulate combinber null-value term a next b)
  (if (> a b)
    null-value
    (combinber (term a)
       (accumulate combinber null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))
  
(define (product term a next b)
  (accumulate * 1 term a next b))

; test
(define (inc n) (+ n 1))

(define (identify n) n)

; 階乗：factorial
; (factorial 1 5)
; = 1 * 2 * 3 * 4 * 5 = 120
(define (factorial a b)
  (product identify a inc b))


;---------------------------
; 反復
;---------------------------

; 足し算
;(define (sum term a next b)
;  (define (iter a result)
;    (if (> a b) 
;      result
;      (iter (next a) (+ result (term a)))))
;  (iter a 0))

; 掛け算
;(define (product term a next b)
;  (define (iter a result)
;    (if (> a b) 
;      result
;      (iter (next a) (* result (term a)))))
;  (iter a 1))

(define (accumulate combinber null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combinber result (term a)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

