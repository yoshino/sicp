;(define (prime-sum-pair list1 list2)
;  (let ((a (an-element-of list1))
;        (b (an-element-of list2)))
;    (require (prime? (+ a b)))
;    (list a b)))

; requireが真を返す時は空ではないambのとき
(define (require p) (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  ; 曖昧にリストの最初の要素化残りから１つの要素を返す
  (amb (car items)
       (an-element-of (cdr items))))

; n以上の整数を返す
(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

;-----------------------------------------------------------
;2つの与えられた限界の間の整数を返す手続き an-integer-between
;-----------------------------------------------------------
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high )))
    (let ((j (an-integer-between i high )))
      (let ((k (an-integer-between j high )))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k )))))

(define (an-integer-between low high)
  (require (< low high))
  (amb low (an-integer-between (+ low 1) high)))



