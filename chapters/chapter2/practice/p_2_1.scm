; コンストラクタ
(define (make-rat n d)
  (cond
    ((and (> n 0) (> d 0)) (cons n d))
    ((and (< n 0) (< d 0)) (cons (* n -1) (* d -1)))
    ((< n 0) (cons n d))
    ((< d 0) (cons (* n -1) (* d -1)))))

; gosh> (make-rat 4 5)
; (4 . 5)
; gosh> (make-rat -4 5)
; (-4 . 5)
; gosh> (make-rat 4 -5)
; (-4 . 5)
; gosh> (make-rat -4 -5)
; (4 . 5)
