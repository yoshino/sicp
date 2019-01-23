;gosh> (load "./amb")

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (prime? n)
  (= n ( smallest-divisor n)))

(define (smallest-divisor n) (find-divisor n 2))
(define (divides? a b) (= (remainder b a) 0))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require ( prime? (+ a b)))
    (list a b)))

(let ((pairs '()))
  (if-fail
    (let ((p ( prime-sum-pair '(1 3 5 8)
                              '(20 35 110))))
      (permanent-set! pairs (cons p pairs))
      (amb)) ; ambは失敗を意味するのでバックトラックしてprime-sum-pairを検索する
    pairs))


;;; Amb-Eval input:
(let ((pairs '()))
  (if-fail
    (let ((p (prime-sum-pair '(1 3 5 8)
                              '(20 35 110))))
      (permanent-set! pairs (cons p pairs))
      (amb))
    pairs))

;;; Starting a new problem
;;; Amb-Eval value:
((3 20) (3 35) (3 110) (8 20) (8 110) (8 35) (5 35) (5 110) (5 20) (1 110) (1 35) (1 20))


;;; Starting a new problem
;;; Amb-Eval value:
((8 110) (8 35) (8 20) (5 35) (5 110) (5 20) (3 35) (3 110) (3 20) (1 35) (1 110) (1 20))

;;; Starting a new problem
;;; Amb-Eval value:
((5 35) (5 110) (5 20) (8 20) (8 110) (8 35) (3 20) (3 35) (3 110) (1 20) (1 110) (1 35))
