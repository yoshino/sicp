; コンストラクタ
(define (make-rectangle top-left top-right bottom-left bottom-right)
  (cons (cons top-left top-right) (cons bottom-left bottom-right)))

; 長方形
(define h (cons 0 1))
(define i (cons 1 1))
(define j (cons 0 0))
(define k (cons 1 0))

(define r (make-rectangle h i j k))

; セレクタ
; 幅
(define (rect-width rect)
  (- (car (cdr (car rect))) (car (car (car rect)))))

; 高さ
(define (rect-height rect)
  (- (cdr (car (car rect))) (cdr (car (cdr rect)))))

; 外周
(define (calc-outer rect)
  (+ (* 2 (rect-width rect)) (* 2 (rect-height rect))))

; gosh> (calc-outer (cons 0 1) (cons 1 1) (cons 0 0) (cons 1 0))
; 0

; 面積
(define (calc-area rect)
  (* (rect-width rect) (rect-height rect)))
