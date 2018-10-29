; normalバージョン
(define (expmod base exp m)
   (cond ((= exp 0) 1)
         ((even? exp)
          (remainder (square (expmod base (/ exp 2) m))
                     m))
         (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

; square関数を置き換えたもの。
(define (expmod base exp m)
   (cond ((= exp 0) 1)
         ((even? exp)
          (remainder (* (exmod base (/ exp 2) m) (expmod base (/ exp 2) m)))
                     m))
         (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))
; (expmod 4 16 16)
; (R (* (expmod (4 8 16) 16) (expmod 4 8 16)) 16)
; nの増加に従って計算量が線形に増加していくことがわかる。

; もう計算するが大変なのでここでストップ
; 最初のバージョンが以下のように展開するのに比較

; (expmod 4 16 16)
; (R (S (expmod 4 8 16) 16))
; (R (S (R (S (expmod 4 4 16) 16) 16)))
; (R (S (R (S (R (S (expmod 4 2 16) 16) 16) 16))))
; (R (S (R (S (R (S (R (S (expmod 4 1 16) 16) 16) 16) 16))))
; (R (S (R (S (R (S (R (S (R (*4 (expmod 4 0 16) 16) 16) 16) 16) 16))))
; (R (S (R (S (R (S (R (S (R 4 16) 16) 16) 16) 16))))
; (R (S (R (S (R (S (R (S 4 16) 16) 16) 16))))
; (R (S (R (S (R (S (R 16 16) 16) 16) 16))))
