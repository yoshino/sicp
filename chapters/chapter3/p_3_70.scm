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

; 3.56より
; merge
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
               (cond ((< s1car s2car)
                      (stream-cons s1car (merge (stream-cdr s1) s2)))
                     ((> s1car s2car)
                      (stream-cons s2car (merge s1 (stream-cdr s2))))
                     (else
                       (stream-cons s1car
                                    (merge (stream-cdr s1)
                                           (stream-cdr s2)))))))))


; 重み関数を使ってparisを適切な順序に並び替えたい
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


;a
(define (add-pairs-weight pair)
  (+ (car pair) (cadr pair)))
(define p (weighted-pairs integers integers add-pairs-weight))

;b
(define (filter-235 s)
  (and (not (= (remainder (car l) 2) 0))
       (not (= (remainder (car l) 3) 0))
       (not (= (remainder (car l) 5) 0))))

(define (mult-pairs-weight pair)
  (+ (* 2 (car pair)) (* 3 (cadr pair)) (* 5 (car pair) (cadr pair))))

(define p (weighted-pairs
            (stream-filter filter-235 integers)
            (stream-filter filter-235 integers)
            (stream-filter filter-235 integers) mult-pairs-weight))
