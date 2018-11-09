(define (start-segment (cons n p)))
(define (end-segment (cons n p)))

(define (x-point segment) (car segment))
(define (y-point segment) (cdr segment))

; コンストラクタ
(define (make-segment start-segment end-segment)
  (cons start-segment end-segment))

; 中点を求める
(define (midpoint-segment start-segment end-segment)
  (make-segment
    (/ (+ (x-point start-segment) (x-point end-segment)) 2)
    (/ (+ (y-point start-segment) (y-point end-segment)) 2)))



(define (calc-distance start-segment end-segment)
  (sqrt (+
          (square (- (x-point start-segment) (x-point end-segment)))
          (square (- (y-point start-segment) (y-point end-segment))))))
