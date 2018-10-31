; 原型
(define (accumulate combinber null-value term a next b)
  (if (> a b)
    null-value
    (combinber (term a)
       (accumulate combinber null-value term (next a) next b))))


(define (inc n) (+ n 1))

(define (square n) (* n n))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-devisor)
  (cond ((> (square test-devisor) n) n)
        ((divides? test-devisor n) test-devisor)
        (else (find-divisor n (+ test-devisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


; Filter関数の追加
(define (accumulate combinber filter null-value term a next b)
  (if (> a b)
    null-value
    (if (filter a)
      (combinber (term a) (accumulate combinber filter null-value term (next a) next b))
      (accumulate combinber filter null-value term (next a) next b))))

(define (sum-prime-square a b)
  (accumulate + prime? 0 square a inc b))


; gosh> (sum-prime-square 1 10)
; 1 + 4 + 9 + 25 + 49
; 88
; gosh> (sum-prime-square 1 20)
; 1 + 4 + 9 + 25 + 49 + 121 + 169 + 289 + 361
; 1028
