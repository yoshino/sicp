(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

; (sine 12.15)
; (p (sine 12.15/3))
; (p (sine 4.05/3))
; (p (sine 1.35/3))
; (p (sine 0.45/3))
; (p (sine 0.15/3))
;
; よって５回実行されている。

; 増加オーダーに関して
; スペースは線形に増える
; 具体的に増えるのは１つのstepだけ

; 必要な計算の回数(k)は、3**kに依存していることがわかる。
; このことを言い換えるのならば、n = 3**kになるときまで計算するということである。
; 底に３をとり両辺に対数をとると、k = log3nになる。
; よって必要な計算回数はlognに近似できる。
; 増加オーダーはO(logn)

; このあたりの解説は以下がわかりやすかった。
; https://qiita.com/cotrpepe/items/1f4c38cc9d3e3a5f5e9c
