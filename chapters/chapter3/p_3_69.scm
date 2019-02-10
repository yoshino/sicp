(use util.stream)

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

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
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
  (stream-cons
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (x) (cons (stream-car s) x))
                  (pairs (stream-cdr t) (stream-cdr u)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (pitagoras? triples)
  (= (+ (expt (car triples) 2) (expt (cadr triples) 2)) (expt (caddr triples) 2)))

(define ts (stream-filter pitagoras? (triples  integers integers integers)))

;gosh> (stream-head ts 10)
;(1 1 1)
;(1 2 2)
;(2 2 2)
;(1 2 3)
;(2 3 3)
;(1 3 3)
;(3 3 3)
;(1 2 4)
;(2 3 4)
;(1 3 4)
