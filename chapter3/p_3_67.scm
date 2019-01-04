(use util.stream)

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(define integers2 (integers-starting-from 1))

(define (stream-head s n)
  (define counter 0)
  (define (iter s)
    (if (<= n counter)
      'done
      (begin
        (display (stream-car s))
        (newline)
        (set! counter (+ counter 1))
        (iter (stream-cdr s)))))
  (iter s))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (stream-cons
    (list (stream-car s) (stream-car t))
    (interleave
      (interleave
        (stream-map (lambda (x) (list (stream-car t) x))
                    (stream-cdr s))
        (stream-map (lambda (x) (list x (stream-car s)))
                    (stream-cdr t)))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define pairs-of-integers (pairs integers integers))

;(define test (stream-map (lambda (x) (list (stream-car integers) x)) (stream-cdr integers2)))
;define test2 (stream-map (lambda (x) (list (stream-car integers2) x)) (stream-cdr integers)))
;stream-head test 10)
;stream-head test2 10)
;1 2)
;1 3)
;1 4)
;1 5)
;1 6)
;1 7)
;1 8)
;1 9)
;1 10)
;1 11)
;1 2)
;1 3)
;1 4)
;1 5)
;1 6)
;1 7)
;1 8)
;1 9)
;1 10)
;1 11)
