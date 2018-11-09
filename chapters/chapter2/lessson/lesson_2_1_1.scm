(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom y)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom y)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

; コンストラクタ
(define (make-rat n d)
  (cond
    ((and (> n 0) (> d 0)) (cons n d))
    ((and (< n 0) (< d 0)) (cons (* n -1) (* d -1)))
    ((< n 0) (cons n d))
    ((< d 0) (cons (* n -1) (* d -1)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; gosh> (make-rat 4 5)
; (4 . 5)
; gosh> (make-rat -4 5)
; (-4 . 5)
; gosh> (make-rat 4 -5)
; (-4 . 5)
; gosh> (make-rat -4 -5)
; (4 . 5)
