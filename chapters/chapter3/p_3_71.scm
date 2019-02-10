;(use util.stream)

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (sum-cube x)
  (let ((a (car x))
        (b (cadr x)))
    (+ (* a a a) (* b b b))))

(define (merge-weighted pairs1 pairs2 weight)
  (cond ((stream-null? (stream-car pairs1)) pairs2)
        ((stream-null? (stream-car pairs2)) pairs1)
        (else
          (let ((p1car (stream-car pairs1))
                (p2car (stream-car pairs2)))
               (if (< (weight p1car) (weight p2car))
                   (stream-cons p1car (merge-weighted pairs2 (stream-cdr pairs1) weight))
                   (stream-cons p2car (merge-weighted pairs1 (stream-cdr pairs2) weight)))))))

(define (weighted-pairs s t weight)
  (stream-cons
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      weight)))

(define (ramanujan stream)
  (let ((s1 (stream-car stream))
        (s2 (stream-car (stream-cdr stream))))
    (let ((weight1 (sum-cube s1))
          (weight2 (sum-cube s2)))
      (cond ((= weight1 weight2)
             (stream-cons weight1
                          (ramanujan (stream-cdr stream))))
            (else
             (ramanujan (stream-cdr stream)))))))

(define r (weighted-pairs integers integers sum-cube))

