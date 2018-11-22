(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
    ()
    (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (ordered-triples n)
   (flatmap (lambda (i)
      (flatmap (lambda (j)
         (map (lambda (k)
                (list i j k))
              (enumerate-interval 1 (- j 1))))
       (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

;gosh> (ordered-triples 5)
;((3 2 1) (4 2 1) (4 3 1) (4 3 2) (5 2 1) (5 3 1) (5 3 2) (5 4 1) (5 4 2) (5 4 3))

(define (triple-sum? triple s)
  (= s (accumulate + 0 triple)))

(define (make-triple-sum triple)
  (append triple (list (accumulate + 0 triple))))

(define (ordered-triple-sum n s)
  (define (triple-sum? triple)
    (= s (accumulate + 0 triple)))
  (map make-triple-sum
    (filter triple-sum?
            (ordered-triples n))))

;gosh> (ordered-triple-sum 12 12)
;((5 4 3 12) (6 4 2 12) (6 5 1 12) (7 3 2 12) (7 4 1 12) (8 3 1 12) (9 2 1 12))
