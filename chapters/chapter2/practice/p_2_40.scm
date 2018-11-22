; prime?
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-devisor)
  (cond ((> (square test-devisor) n) n)
        ((divides? test-devisor n) test-devisor)
        (else (find-divisor n (+ test-devisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

; accumulate
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (ennumerate-interval low high)
  (if (> low high)
    ()
    (cons low (ennumerate-interval (+ low 1) high))))
; gosh> (ennumerate-interval 1 5)
;(1 2 3 4 5)
;gosh> (ennumerate-interval 1 3)
;(1 2 3)

; prime-sum
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                            (lambda(i) (map (lambda(j) (list i j)) (ennumerate-interval 1 (- i 1))))
                            (ennumerate-interval 1 n)))))
;gosh> (prime-sum-pairs 5)
;((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7))

; (filter prime-sum? (flatmap (lambda(i) (map (lambda(j) (list i j)) (ennumerate-interval 1 (- i 1)))) (ennumerate-interval 1 3)))
;gosh> (filter prime-sum? (flatmap (lambda(i) (map (lambda(j) (list i j)) (ennumerate-interval 1 (- i 1)))) (ennumerate-interval 1 3)))
;((2 1) (3 2))
;gosh> (filter prime-sum? (flatmap (lambda(i) (map (lambda(j) (list i j)) (ennumerate-interval 1 (- i 1)))) (ennumerate-interval 1 5)))
;((2 1) (3 2) (4 1) (4 3) (5 2))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))
;(flatmap (lambda(i) (map (lambda(j) (list i j)) (ennumerate-interval 1 (- i 1))))
;         (ennumerate-interval 1 3)
;((2 1) (3 1) (3 2))
;gosh> (flatmap (lambda(i) (map (lambda(j) (list i j)) (ennumerate-interval 1 (- i 1))))
;               (ennumerate-interval 1 5))
;((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4))

; 展開
;(flatmap (lambda(i) (map (lambda(j) (list i j)) (ennumerate-interval 1 (- i 1))))
;  (ennumerate-interval 1 5))
;(flatmap (lambda(i) (map (lambda(j) (list i j)) (ennumerate-interval 1 (- i 1)))) (1 2 3 4 5))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
; gosh> (prime-sum? (list 1 2))
; #t
; gosh> (prime-sum? (list 3 5))
; #f

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
;gosh> (make-pair-sum (list 1 2))
;(1 2 3)
;gosh> (make-pair-sum (list 3 4))
;(3 4 7)

;; ここから解答
(define (unique-pairs n)
   (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))
