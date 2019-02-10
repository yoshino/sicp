(define (mul-inner-val value)
  (lambda (x)
      (begin (set! value (* value x))
             value)))

(define f (mul-inner-val 1))

;順番が大事
;gosh> (f 1)
;1
;gosh> (f 1)
;1
;gosh> (f 0)
;0
;gosh> (f 1)
;0

;gosh> (+ (f 0) (f 1))
;0
;gosh> (+ (f 1) (f 0))
;0

;gosh> (+ (f 1) (f 0))
;1
