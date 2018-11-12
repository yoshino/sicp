; r1*r2 / (r1 + r2)
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

; 1/(1/r1 + 1/r2)
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
  (div-interval
    one (add-interval (div-interval one r1)
                    (div-interval one r2)))))

; par2はpar1よりも良いプログラム？？
; 不確定な数値が数値を表す変数が繰り返し出てこないことが理由らしい。

(define (div-interval x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)      ; 不確定な値！
                   (max p1 p2 p3 p4))))   ; 不確定な値！

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2-14より、掛け算と割り算が等価ではない
; よって、掛け算、割り算を使用するのが少ないpar2の方が優秀。
