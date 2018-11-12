; 演算系
(define (add-interval x y)
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
(define (lower-bound a) (cdr a))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
   (* 100.0 (/ (width i) (center i))))

(define (make-center-width c w)
   (make-interval (- c w) (+ c w)))

(define (make-center-percent c p)
   (make-center-width c (* c (/ p 100.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

; mulの定義が適切でないのでpar1とpar2に違いが生じている
; たぶん
; gosh> (par1 (make-center-percent 10 1) (make-center-percent 20 1))
; (6.469306930693071 . 6.869360269360269)
; gosh> (par2 (make-center-percent 10 1) (make-center-percent 20 1))
; (6.6 . 6.7333333333333325)
