; accumulate
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

; count-leaves
(define (count-leaves t)
  (accumulate
    (lambda (x y)
      (if (not (pair? x))
        (+ 1 y)
        (count-leaves x)))
    0
    t))

(define l (list 1 2 3 (list 2 4 5)))
