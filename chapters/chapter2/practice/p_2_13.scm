; pair: x, yの要素が全て正であるとする。
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval p1 p4)))

; コンストラクタ
(define (make-interval a b) (cons a b))

; セレクタ
(define (upper-bound a) (car a))
(define (lower-bound a) (cdr a))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

; percent
(define (make-center-percent c p)
  (make-interval (- c (percent c p)) (+ c (percent c p))))

; percentから距離に変換
(define (percent c p) (* c (* p 0.01)))

; x: cx-cx*px, cx+cx*px
;    (cx*(1-px)), (cx*(1+px))
; y: cy-cy*py, cy+cy*py
;    (cy*(1-py)), (cy*(1+py))

;p1: cx*cy*(1-px)*(1-py)
;    cx*cy-(px+py)
;p4: cx*cy*(1+px)*(1+py)
;    cx*cy+(px+py)

