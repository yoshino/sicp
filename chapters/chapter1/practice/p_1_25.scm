(define (expmod base exp m)
   (cond ((= exp 0) 1)
         ((even? exp)
          (remainder (square (expmod base (/ exp 2) m))
                     m))
         (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

; 素数でなくフェルマーのテストをパスしない。
; (expmod 4 16 16)
; (R (S (expmod 4 8 16) 16))
; (R (S (R (S (expmod 4 4 16) 16) 16)))
; (R (S (R (S (R (S (expmod 4 2 16) 16) 16) 16))))
; (R (S (R (S (R (S (R (S (expmod 4 1 16) 16) 16) 16) 16))))
; (R (S (R (S (R (S (R (S (R (*4 (expmod 4 0 16) 16) 16) 16) 16) 16))))
; (R (S (R (S (R (S (R (S (R 4 16) 16) 16) 16) 16))))
; (R (S (R (S (R (S (R (S 4 16) 16) 16) 16))))
; (R (S (R (S (R (S (R 16 16) 16) 16) 16))))

; 16が素数でない場合はexpmodは0を返す。
; 上の計算結果より、(R 16 16) = 0なので、
; それ以降の計算は、(R 0 16) = 0 を繰り返すだけなので、
; 計算時間はかからないことが容易に想像がつく

; 素数でありフェルマーのテストをパスする。
; (expmod 4 13 13)
; (R (* 4 (expmod 4 12 13) 13))
; (R (* 4 ( R (S (expmod 4 6 13) 13) 13))
; (R (* 4 ( R (S (R (S (expmod 4 3 13) 13) 13) 13))
; (R (* 4 ( R (S (R (S (* 4 (expmod 4 2 13) 13) 13) 13) 13))
; (R (* 4 ( R (S (R (S (* 4 (R (S (expmod 4 1 13) 13) 13) 13) 13) 13))
; (R (* 4 ( R (S (R (S (* 4 (R (S (R (* 4 (expmod 4 0 13) 13) 13) 13) 13) 13) 13))
; (R (* 4 ( R (S (R (S (* 4 (R (S (R 4 13) 13) 13) 13) 13) 13))
; (R (* 4 ( R (S (R (S (* 4 (R 16 13) 13) 13) 13) 13))

; ここから計算を展開していくけれど、計算の対象が余りなので、計算量は多くならない。

; 次にRをするのは最初のときだけという簡略バージョンをみていく。
(define (fast-expt b n)
   (cond ((= n 0) 1)
         ((even? n) (square (fast-expt b (/ n 2))))
         (else (* b (fast-expt b (- n 1))))))

; 素数でなくフェルマーのテストをパスしない。
; (expmod 4 16 16)
; (R (fast-expt 4 16) 16)
; (R (S (fast-expt 4 2) 16)
; (R (S (S (fast-expt 4 1) 16)
; (R (S (S (* 4 (fast-expt 4 0) 16)
; (R (S (S (* 4 1) 16)

; この時の計算量は上の時と変わらない。

; 素数でありフェルマーのテストをパスする。
; (expmod 4 13 13)
; (R (fast-expt 4 13) 13)
; (R (* 4 (fast-expt 4 12) 13)
; (R (* 4 (S (fast-expt 4 6) 13)
; (R (* 4 (S (S (fast-expt 4 3) 13)
; (R (* 4 (S (S (* 4 (fast-expt 4 2) 13)
; (R (* 4 (S (S (* 4 ( (S (fast-expt 4 1) 13)
; (R (* 4 (S (S (* 4 ( (S (* 4 (fast-expt 4 0) 13)

;最終的にはこの計算をして最後に13で割ったあまりを求める。
; (R (* 4 (S (S (* 4 ( (S (* 4 1))))))) 13)

; 計算量は、”ｂ”あるいは”ｎ"が大きくなるに連れて大きくなっていくのがわかる
