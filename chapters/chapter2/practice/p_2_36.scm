(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    ()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define l1 (list 1 2 3))
(define l2 (list 4 5 6))
(define l3 (list 7 8 9))
(define l4 (list 10 11 12))
(define l5 (list l1 l2 l3 l4))

;<gosh> (map car l5)
;(1 4 7 10)
;gosh> (map cdr l5)
;((2 3) (5 6) (8 9) (11 12))
