(define (add_interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub_interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))

; コンストラクタ
(define (make-interval a b) (cons a b))

; セレクタ
(define (upper-bound a) (car a))
(define (lower-bound a) (cdr a))i

; width
(define (width up low)
  (/ (- up low) 2))

; width
; w = (u-l)/2

; をこのように定義すると、

; w1 = (u1-l1)/2
; w2 = (u2-l2)/2

; add_intervalの結果はこのようになるので、
; wで表すことができる
; add_w = ((u1+u2)-(l1+12))/2
;       = (u1-l1)/2 + (u2-l2)/2
;       = w1 + w2

; sub_w = ((u1-u2)-(l1-12))/2
;       = (u1-l1)/2 - (u2-l2)/2
;       = w1 - w2

; 乗算の結果に関して考える
;((min p1 p2 p3 p4) (max p1 p2 p3 p4))

;(let ((p1 (* (lower-bound x) (lower-bound y)))
;      (p2 (* (lower-bound x) (upper-bound y)))
;      (p3 (* (upper-bound x) (lower-bound y)))
;      (p4 (* (upper-bound x) (upper-bound y))))
;x
; (min p1 p2 p3 p4)
; (min l1*l2 l1*u2 u1*l2 u1*u2) => minの計算 => lower-bound
; lower-bound => ????? => 
