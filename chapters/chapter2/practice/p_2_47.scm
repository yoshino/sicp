(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define fr (list (list 0 0) (list 2 2) (list 3 3)))

(define (origin-frame f) (car f))
(define (edge1-frame f) (car (cdr f)))
(define (edge2-frame f) (car (cdr (cdr f))))

;gosh> (origin-frame fr)
;(0 0)
;gosh> (edge1-frame fr)
;(2 2)
;gosh> (edge2-frame fr)
;(3 3)

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define fr (make-frame (list 0 0) (list 2 2) (list 3 3)))

(define (origin-frame f) (car f))
(define (edge1-frame f) (car (cdr f)))
(define (edge2-frame f) (cdr (cdr f)))
