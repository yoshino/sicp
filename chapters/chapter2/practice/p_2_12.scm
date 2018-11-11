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

; percentから距離に変換する
(define (percent c p) (* c (* p 0.01)))

; center+-5.0%
; gosh> (make-center-percent (center (cons 1 10.0)) 5.0)
; (5.225 . 5.775)
