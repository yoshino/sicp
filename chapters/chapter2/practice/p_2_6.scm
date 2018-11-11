(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
   (lambda (f) (lambda (x) (f ((n f) x)))))

; チャーチル数
;  zero
(define (inc n) (+ n 1))
; gosh> ((zero inc) 0)
; 0
; gosh> ((zero inc) 1)
; 1
; gosh> ((zero inc) 2)
; 2

; one
; (add-1 zero)
; (lambda (f) (lambda (x) (f ((zero f) x))))
; (lambda (f) (lambda (x) (f x)))
(define one
  (lambda (f) (lambda (x) (f x))))

; two
; (add-1 one)
; (lambda (f) (lambda (x) (f ((one f) x))))
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
; (lambda (f) (lambda (x) (f ((f x)))))
(define two
  (lambda (f) (lambda (x) (f (f x)))))

; gosh> ((one inc) 3)
; 4
; gosh> ((two inc) 3)
; 5
