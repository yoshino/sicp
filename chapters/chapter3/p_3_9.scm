(define (factorial n) (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* counter product)
               (+ count 1)
               max-count)))

;-----------------------------
; E1

; n : 1
;-----------------------------
;body
;(fact-iter 1 1 n)

; nが束縛されている
; fact-iterに対する束縛がないのでグローバル環境に探しに行く.

; global環境にfact-iterをみつけ、
; 新しい環境を作成する

;-----------------------------
; E2

; product : 1
; counter : 1
; max-counter :6
;-----------------------------
;body
;(if (> counter max-count)
;  product
;  (fact-iter (* counter product)
;             (+ count 1)
;             max-count)))

; この中でE2環境で束縛されていないのは。fact-iterのみ。
; fact-iterをグローバルに探しに行く。

; 以降、この繰り返し。
