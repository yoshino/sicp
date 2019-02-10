(use util.stream)

; --------------------------------------------------
; 代入を利用した
; モンテカルロ積分
; --------------------------------------------------
(define (random-in-range low high)
  (let ((range (+ (- high low))))
    (+ low (random-integer range))))

(define (estimate-integral p x1 x2 y1 y2 trial)
  (define (rectangle-area x1 x2 y1 y2)
    (* (- x2 x1) (- y2 y1)))
  (* (monte-carlo trial (p x1 x2 y1 y2)) (rectangle-area x1 x2 y1 y2)))

(define (p x1 x2 y1 y2)
  (define p1 (random-in-range x1 x2))
  (define p2 (random-in-range y1 y2))
  (define r1 (/ (+ x1 x2) 2.0))
  (define r2 (/ (+ y1 y2) 2.0))
  (define r  (- r1 x1))
  (define d (+ (square (- p1 r1)) (square (- p2 r2))))
  (or (< d (expt r 2)) (= d (expt r 2))))

(define (estimate-integral p x1 x2 y1 y2 trial)
  (define rectangle-area
    (* (- x2 x1) (- y2 y1) 1.0))
  (define (circle-in-rectangle?) (p x1 x2 y1 y2))
  (* (monte-carlo trial circle-in-rectangle?) rectangle-area))

; --------------------------------------------------
; ストリームを利用した
; モンテカルロ積分
; --------------------------------------------------
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (stream-cons
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define ones (stream-cons 1 ones))

(use srfi-27)
(define (random x)
        ; (random-integer x))
        (* (random-real) x))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (random-in-range-stream low high)
  (stream-map
    (lambda (x) (random-in-range low high))
    ones))

(define (estimate-integral pred x1 x2 y1 y2)
  (define x-stream (random-in-range-stream x1 x2))
  (define y-stream (random-in-range-stream y1 y2))
  (stream-map
   (lambda (p)
     (* (* (- x2 x1) (- y2 y1)) p))
   (monte-carlo
    (stream-map pred x-stream y-stream) 0 0)))

(define (point-in-circle? x y cx cy r)
  (<= (+ (square (- x cx)) (square (- y cy))) (square r)))

(define (test-check x y)
  (point-in-circle? x y 5.0 7.0 3.0))

(stream-ref (estimate-integral test-check 2.0 8.0 4.0 10.0) 10)

;gosh> (stream-ref (estimate-integral test-check 2.0 8.0 4.0 10.0) 10)
;32.72727272727273
;gosh> (stream-ref (estimate-integral test-check 2.0 8.0 4.0 10.0) 100)
;29.584158415841582
